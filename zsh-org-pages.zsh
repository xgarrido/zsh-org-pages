# -*- mode: shell-script; -*-
#
# Copyright (C) 2014 Xavier Garrido
#
# Author: xavier.garrido@gmail.com
# Keywords: org, html
# Requirements: pkgtools
# Status: not intended to be distributed yet

typeset -g __ogp_path=$(dirname $0)
typeset -gA __ogp_color_map
__ogp_color_map=(
    green     \#67ad00
    blue      \#3399cc
    darkblue  \#003399
    yellow    \#f1c40f
    orange    \#e67e22
    red       \#e74c3c
    turquoise \#1abc9c
    violet    \#8e44ad
)

function org-pages ()
{
    pkgtools::default_values
    pkgtools::at_function_enter org-pages

    local append_list_of_options_arg
    local append_list_of_cmd_arg
    local generate_pdf=false
    local generate_html=true
    local clean=false
    local generate=false
    local publish=false
    local recursive=false
    local keep_tmp_files=false
    local generate_floating_footnote=true
    local generate_home_link=false
    local generate_github_link=false
    local generate_org_link=false
    local convert_images=true
    local replace_pdf_ext=true
    local color_scheme="default"
    local template="default"
    local tmp_dir=/tmp/org-pages.d
    while [ -n "$1" ]; do
        local token="$1"
        if [[ "${token[0,1]}" == "-" ]]; then
            local opt=${token}
            if [[ ${opt} == "-h" || ${opt} == "--help" ]]; then
                return 0
            elif [[ ${opt} == "-d" || ${opt} == "--debug" ]]; then
                pkgtools::msg_using_debug
            elif [[ ${opt} == "-D" || ${opt} == "--devel" ]]; then
                pkgtools::msg_using_devel
            elif [[ ${opt} == "-v" || ${opt} == "--verbose" ]]; then
                pkgtools::msg_using_verbose
            elif [[ ${opt} == "--recursive" ]]; then
                recursive=true
            elif [[ ${opt} == "--pdf" ]]; then
                generate_pdf=true
                generate_html=false
            elif [[ ${opt} == "--html" ]]; then
                generate_html=true
                generate_pdf=false
            elif [[ ${opt} == "--no-floating-footnote" ]]; then
                generate_floating_footnote=false
            elif [[ ${opt} == "--generate-home-link" ]]; then
                generate_home_link=true
            elif [[ ${opt} == "--generate-git-link" ]]; then
                generate_git_link=true
            elif [[ ${opt} == "--generate-org-link" ]]; then
                generate_org_link=true
            elif [[ ${opt} == "--no-image-conversion" ]]; then
                convert_images=false
            elif [[ ${opt} == "--keep-tmp-files" ]]; then
                keep_tmp_files=true
            elif [[ ${opt} == "--do-not-replace-pdf-ext" ]]; then
                replace_pdf_ext=false
            elif [[ ${opt} == --color* ]]; then
                color_scheme=$(echo ${opt} | sed 's/--color.*=//')
            elif [[ ${opt} == "--template" ]]; then
                shift 1
                template=$1
            else
                append_list_of_options_arg+="${opt} "
            fi
        else
            if [[ ${token} == "clean" ]]; then
                clean=true
            elif [[ ${token} == "generate" ]]; then
                generate=true
            elif [[ ${token} == "publish" ]]; then
                publish=true
                generate=true
                generate_html=true
                generate_pdf=false
            elif [[ "x${token}" != "x" ]]; then
                append_list_of_cmd_arg+="${token} "
            fi
        fi
        shift
    done

    # append_list_of_cmd_arg=$(echo ${append_list_of_cmd_arg} | sed 's/:/:\"/g')
    # append_list_of_options_arg=$(echo ${append_list_of_options_arg} | sed 's/=/=\"/g')

    pkgtools::msg_devel "generate=${generate}"
    pkgtools::msg_devel " |- pdf=${generate_pdf}"
    pkgtools::msg_devel " |- html=${generate_html}"
    pkgtools::msg_devel "publish=${publish}"
    pkgtools::msg_devel "color scheme=${color_scheme}"
    pkgtools::msg_devel "append_list_of_cmd_arg=${append_list_of_cmd_arg}"
    pkgtools::msg_devel "append_list_of_options_arg=${append_list_of_options_arg}"

    if ${clean}; then
        pkgtools::msg_notice "Cleaning directory"
        if [ -d doc ]; then
            rm -rf doc
        fi
        if [ -f toc.pdf ]; then
            rm -f toc.*
        fi
        pkgtools::at_function_exit
        return 0
    fi

    if ! ${generate}; then
        pkgtools::msg_error "No output file will be generated !"
        pkgtools::at_function_exit
        return 1
    fi

    pkgtools::msg_notice "Start export process..."

    op::prepare_process
    op::process
    op::post_process

    pkgtools::msg_notice "Export successfully done"

    if ${publish}; then
        pkgtools::msg_notice "Publishing to the web"
	find doc -name *.*~ -exec rm -f {} \;
        find -type d -empty -path "*doc*" -exec rm -rf {} \;
	(cd doc/html && tar czvf /tmp/org-publish.tar.gz .)
        rm -rf doc
        current_branch_name=$(git branch | grep '*' | awk '{print $2}')
	git checkout gh-pages
	tar xzvf /tmp/org-publish.tar.gz
	if [ -n "`git status --porcelain`" ]; then git commit -am "update doc" && git push; fi
	git checkout ${current_branch_name}
    fi

    unset generate_pdf generate_html
    unset color_scheme
    unset clean
    unset generate
    unset publish
    unset recursive
    unset keep_tmp_files
    unset generate_floating_footnote
    pkgtools::at_function_exit
    return 0
}

function op::prepare_process()
{
    pkgtools::at_function_enter op::prepare_process

    if [[ $template == "twbs" ]]; then
        (
            mkdir -p ${tmp_dir}
            cd ${tmp_dir}
            wget https://raw.githubusercontent.com/marsmining/ox-twbs/master/ox-twbs.el
        )
    fi

    __split_file()
    {
        pkgtools::msg_notice "Split file $1"
        cat $1 | awk -v current=$1 '
                         BEGIN{j=-1}
                         {
        		     if ($1 == "*") {
                                     if (match($0, "COMMENT")) comment=1
                                     else {
                                          comment=0
        			          j++
        			          heading[j]=substr($0,3)
                                     }
        			 } else {
                                     if (comment == 0)
        			          text[j]=text[j]"\n"substr($0,1)
        			 }
        		 }
                         END{
                             print "#+HTML: <div id=\"text-table-of-contents\">" > "toc.org"
                             for (i in heading) {
                                     current_filename=current
                                     sub(".org", "_"i".org", current_filename)
                                     printf("- [[./%s][%s]]\n", current_filename, heading[i]) >> "toc.org"
                                     sub(".org", ".split.org", current_filename)
                                     print "#+TITLE:", heading[i] > current_filename
                                     print "#+OPTIONS: toc:nil"  >> current_filename
                                     print "#+HTML: <div style=\"display:none;\">" >> current_filename
                                     print "#+INCLUDE: toc.org"  >> current_filename
                                     print "#+HTML: </div>" >> current_filename
        			     print text[i] >> current_filename
        			 }
                             print "#+HTML: </div>" >> "toc.org"
        		 }'
        mv $1 $1.noexport
    }

    pkgtools::msg_debug "Parsing org files..."
    local org_files
    org_files=$(find . -name "*.org")
    for file in ${=org_files}; do
        if [ -L $file ]; then
            pkgtools::msg_debug "$file is a symbolic link"
            continue
        fi
        if ${generate_html}; then
            \cp $file $file.save
            if ${replace_pdf_ext}; then
                sed -i -e 's@\.pdf@\.png@g' $file
            fi
        fi
        if ${generate_pdf}; then
            if [ ${color_scheme} != "default" ]; then
                \cp $file $file.save
                if [ ${color_scheme[0,1]} = "#" ]; then
                    if [ ${#color_scheme} -eq 7 ]; then
                        sed -i -e '1i#+LATEX_HEADER_EXTRA: \\definecolor{default}{HTML}{'${color_scheme:1:7}'}' $file
                    else
                        pkgtools::msg_error "The HEX code is not allowed !"
                    fi
                elif [ ${__ogp_color_map[${color_scheme}]+_} ];then
                    sed -i -e '1i#+LATEX_HEADER_EXTRA: \\definecolor{default}{HTML}{'${__ogp_color_map[${color_scheme}]:1:7}'}' $file
                fi
            fi
        fi
        if [ $(grep -e "#+OPTIONS.*split:t" -c $file) -eq 1 ]; then
            __split_file $file
        elif [ $(grep -e "#+OPTIONS.*split:html" -c $file) -eq 1 ]; then
            if ${generate_html}; then __split_file $file;fi
        elif [ $(grep -e "#+OPTIONS.*split:pdf" -c $file) -eq 1 ]; then
            if ${generate_pdf}; then __split_file $file;fi
        fi
        touch toc.org
    done
    unset org_files
    pkgtools::at_function_exit
    return 0
}

function op::process()
{
    pkgtools::at_function_enter op::process
    export OGP_EXPORT_DIR=$PWD

    local ogp_path=${__ogp_path}
    local emacs_cmd=""
    local emacs_base_cmd="emacs --batch --no-init-file "
    emacs_base_cmd+="--eval \"(require 'org)\" "
    emacs_base_cmd+="--eval \"(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)(python . t)))\" "
    emacs_base_cmd+="--eval \"(setq org-babel-use-quick-and-dirty-noweb-expansion t)\" "
    emacs_base_cmd+="--eval \"(setq org-confirm-babel-evaluate nil)\" "
    emacs_base_cmd+="--eval \"(setq c-standard-font-lock-fontify-region-function 'font-lock-default-fontify-region)\" "
    emacs_base_cmd+="--eval '(let ((this-directory \""$PWD"/\")) "
    emacs_base_cmd+="(org-babel-load-file \""${ogp_path}"/zsh-org-pages.org\") "
    emacs_base_cmd+="(org-babel-tangle-file \""${ogp_path}"/zsh-org-pages.org\"))' "

    if [[ $template == "twbs" ]]; then
        emacs_base_cmd+="--load ${tmp_dir}/ox-twbs.el --eval \"(require 'ox-twbs)\" "
    fi

    if [ "${append_list_of_cmd_arg}" != "" ]; then
        for file in ${append_list_of_cmd_arg}
        do
            emacs_cmd+=${emacs_base_cmd}
            emacs_cmd+="--funcall org-publish-pdf-alone "
            emacs_cmd+="--visit "$file
            echo ${emacs_cmd} | sh
            emacs_cmd=""
        done
    else
        if [ ! -f README.org ]; then
            pkgtools::msg_warning "Missing README.org file ! Create a tmp one"
            touch README.org
        fi
        emacs_cmd+=${emacs_base_cmd}
        if ${generate_html}; then
            pkgtools::msg_notice "Exporting pages to html..."
            if ${recursive}; then
                if [[ $template == "twbs" ]]; then
                    emacs_cmd+="--funcall org-publish-html-twbs-recursive "
                else
                    emacs_cmd+="--funcall org-publish-html-recursive "
                fi
            else
                if [[ $template == "twbs" ]]; then
                    emacs_cmd+="--funcall org-publish-html-twbs "
                else
                    emacs_cmd+="--funcall org-publish-html "
                fi
            fi
        elif ${generate_pdf}; then
            pkgtools::msg_notice "Exporting pages to pdf (through latex)..."
            emacs_cmd="TEXINPUTS=\""$PWD"/doc/pdf:\$TEXINPUTS\" "
            emacs_cmd+=${emacs_base_cmd}" "
            emacs_cmd+="--funcall org-publish-pdf "
        fi
        emacs_cmd+="--visit \"README.org\" "
    fi

    pkgtools::msg_debug ${emacs_cmd}
    if [ ${__pkgtools__msg_debug} -eq 1 ]; then
        echo $emacs_cmd | sh
    else
        echo $emacs_cmd | sh > /dev/null 2>&1
    fi
    if $(pkgtools::last_command_fails); then
        pkgtools::msg_error "Export has failed !"
        pkgtools::at_function_exit
        return 1
    fi

    unset emacs_base_cmd emacs_cmd
    unset ogp_path
    pkgtools::at_function_exit
    return 0
}

function op::post_process()
{
    pkgtools::at_function_enter op::post_process

    pkgtools::msg_debug "Getting back to original files..."

    for file in $(find . -name "*.org.noexport"); do
        \mv $file ${file/.noexport/}
    done
    for file in $(find . -name "*.org.save"); do
        \mv $file ${file/.save/}
    done
    for file in $(find . -name "*.org"); do
        if [[ "$file" = *".split"* ]]; then
            \rm -f $file
        elif [[ "$file" = *"toc."* ]];then
            \rm  -f $file
        fi
    done

    if ${generate_html}; then
        pkgtools::msg_notice "Tweak html files..."
        for file in $(find doc/html -name "*.html"); do
            pkgtools::msg_debug "Changing css directory depth"
            count="${file//[^\/]}"
            rel_path=
            for ((i=2;i<${#count};i++));do
                rel_path+="../"
            done
            sed -i -e 's@href="css/@href="'${rel_path}'css/@g' $file
            if ! ${recursive}; then
                sed -i -e 's@img src="@img src="'${rel_path}'@g' $file
            fi

            pkgtools::msg_debug "Changing some unicode symbol"
            sed -i \
                -e 's@ding{192}@\(\\unicode{x2460}\\)@g' \
                -e 's@ding{193}@\(\\unicode{x2461}\\)@g' \
                -e 's@ding{194}@\(\\unicode{x2462}\\)@g' \
                -e 's@ding{195}@\(\\unicode{x2463}\\)@g' \
                -e 's@ding{42}@\(\\unicode{x261B}\\)@g'  \
                -e 's@cmark@\(\\unicode{x2713}\\)@g' \
                -e 's@xmark@\(\\unicode{x2717}\\)@g' \
                $file

            pkgtools::msg_debug "Changing preamble home link"
            if ${generate_home_link}; then
                if [ $(basename $file) = "index.html" ]; then
                    sed -i -e 's@__home_link__@@g' $file
                fi
                sed -i -e 's@__home_link__@<a href="'${rel_path}'index.html"><i class=\"fa fa-home\"></i></a><br/>@g' $file
            else
                sed -i -e 's@__home_link__@@g' $file
            fi

            local cvs_version
            local cvs_path
            local cvs_branch
            if [ -d .git ]; then
                cvs_path=$(git config --get remote.origin.url | sed -e 's#git@\(.*\):#https://\1/#' -e 's#\.git##')
                if [ "${cvs_path}" = "" ]; then
                    pkgtools::msg_debug "Using git svn info"
                    cvs_path=$(git svn info | grep URL: | awk '{print $2}')
                    cvs_version="revision <a href=\""${cvs_path}"\">"$(git svn info | grep Revision: | awk '{print $2}')"</a> - "$(git svn info | grep Date: | awk '{print $4}')
                else
                    pkgtools::msg_debug "Using git info"
                    cvs_version=$(LC_MESSAGES=en git --no-pager log -1 HEAD --date=short --pretty=format:'commit <a href=\"url/commit/%H\">%h</a> - %ad' \
                        | sed "s#url#"${cvs_path}"#")
                    cvs_branch=$(git branch | grep '*' | awk '{print $2}')
                fi

            fi
            pkgtools::msg_debug "Changing postamble CVS version"
            if  [[ "${cvs_version}" = *"github"* ]]; then
                cvs_icon="fa-github-alt"
            elif  [[ "${cvs_version}" = *"gitlab"* ]]; then
                cvs_icon="fa-gitlab"
            elif [[ "${cvs_version}" = *"git"* ]]; then
                cvs_icon="fa-git"
            fi
            cvs_version="File under <i class=\"fa-brands ${cvs_icon} fa-flip\"></i> version control - ${cvs_version}"
            sed -i -e 's@__cvs_version__@'${cvs_version}'@' $file

            pkgtools::msg_debug "Changing preamble git link"
            if ${generate_git_link}; then
                sed -i -e 's@__github_link__@<a href="'${cvs_path}'"><i class=\"fa-brands '${cvs_icon}'\"></i></a><br/>@g' $file
            else
                sed -i -e 's@__github_link__@@g' $file
            fi
            pkgtools::msg_debug "Changing org link"
            if ${generate_org_link}; then
                filename=$(basename $file)
                if [ $filename = "index.html" ]; then
                    sed -i -e 's@__org_link__@<a href="'${cvs_path/github/raw.githubusercontent}'/'${cvs_branch}'/README.org"><i class=\"fa fa-file-text-o\"></i></a><br/>@g' $file
                fi
                sed -i -e 's@__org_link__@<a href="'${cvs_path/github/raw.githubusercontent}'/'${cvs_branch}'/'${filename/.html/.org}'"><i class=\"fa fa-file-text-o\"></i></a><br/>@g' $file
            else
                sed -i -e 's@__org_link__@@g' $file
            fi

            pkgtools::msg_debug "Changing multirow syntax to rowspan"
            sed -i -e 's/\(<td class.*\)>\(\\multirow{\)\(.*\)}{.*}{\(.*\)}/\1 rowspan="\3">\4/' $file
            sed -i -e 's/\(.*rowspan.*\)\$\(.*\)\$/\1\\(\2\\)/' $file
            cat $file | awk '{
                          if ($0 ~ /<\/table>/) {
                               trcnt = 0
                               has_rowspan = 0
                           }

                           if ($0 ~ /<tr>/) {
                               trcnt++
                               tdcnt = 0
                           }

                           if ($0 ~ /<td/) {
                               tdcnt++
                           }

                           if ($0 ~ /rowspan/) {
                               has_rowspan = 1
                               tdposition = tdcnt
                               trposition = trcnt
                               j = index($0, "rowspan")+9
                               nbr=substr($0, j, index($0, ">")-j-1)-1
                           }

                           if ($0 ~ /xa0/ && has_rowspan) {
                               if (trcnt-trposition <= nbr && tdcnt == tdposition) {
                                   dont_print = 1
                               }
                           } else {
                               dont_print = 0
                           }

                           if (! dont_print) print $0
                           }' > $file.tmp
            mv $file.tmp $file
            sed -i -e 's/.*UNNUMBERED.*//' $file

            unset cvs_path
            unset cvs_version
            unset cvs_branch

            local colors
            for col in ${__ogp_color_map[@]};do colors+="'$col',";done
            sed -i -e 's@__colors__@'${colors%?}'@' $file
            if [ ${color_scheme} = "random" ]; then
                pkgtools::msg_debug "Activating random colors"
                sed -i -e 's@//elem.style.color@elem.style.color@' $file
            fi
            unset colors

            pkgtools::msg_debug "Generate bibliography"
            typeset -A array
            i=1
            for f in $(cat $file | awk -F'[<>]' -v taga="cite" -v tagb="/cite" '{i=1; while (i<=NF) { if ($(i)==taga && $(i+2)==tagb) { print $(i+1) }; i++} }' | tr ',' ' ')
            do
                if [ ${array[$f]+_} ]; then
                    pkgtools::msg_debug "$f entry already found"
                else
                    array[$f]=$i
                fi
                let i++
            done
            sed -i -e 's@(<cite>@[<cite>@g' -e 's@</cite>)@</cite>]@g' $file
            for k in ${(@k)array}
            do
                ref=${array[$k]}
                sed -i -e 's@\\bibitem{'$k'}@<a id="ref.'$ref'" href="#refr.'$ref'">'$ref'</a>@g' $file
                sed -i -e 's@'$k'@<a id="refr.'$ref'" href="#ref.'$ref'">'$ref'</a>@g' $file
                pkgtools::msg_debug "$k --- $ref"
            done
            unset array

            pkgtools::msg_debug "Remove 'split' & 'toc' keywords (if any)"
            if [[ "$file" = *".split."* ]]; then
                \mv $file ${file/.split/}
            elif [[ "$file" = *"toc."* ]];then
                \rm  -f $file
            fi
        done

        pkgtools::msg_notice "Grabbing images"
        files=$(find . -name "*.org")
        if [[ ! -z "${files}" ]]; then
            for file in ${=files}; do
                imgs=$(sed -n '/\[\[.*\(\.jpg\|\.jpeg\|\.png\|\.gif\|\.svg\|.pdf\)\]\]/p' $file | \
                              sed -e 's/\(\[\[\|\]\]\)//g' -e 's/|/\n/g' -e 's/file://g')
                for img in ${=imgs}; do
                    dir=$(dirname $file)
                    img=$dir/$img
                    img_dir=$(dirname $img)
                    if [ -f $img ]; then
                        mkdir -p doc/html/${img_dir}
                        extension=${img##*.}
                        if [[ $extension == "pdf" ]]; then
                            if ${convert_images}; then
                                png=doc/html/${img/.pdf/.png}
                                if [[ ! -a $png || $img -nt $png ]]; then
                                    pkgtools::msg_debug "Convert $img to $png"
                                    convert -density 100 $img $png
                                fi
                            fi
                        else
                            pkgtools::msg_debug "Copying $img to doc/html/${img_dir}..."
                            \cp $img doc/html/${img_dir}
                        fi
                    fi
                done
            done
        fi

        if ${generate_floating_footnote}; then
            pkgtools::msg_notice "Generate floating footnotes"
            pkgtools::msg_debug "Adding special CSS code"
            sed -i -e "\$a.footdef{\nfont-size: 10px;\nright: 0;\nposition: absolute;\nwidth:180px;\n}" doc/html/css/styles.css
            for file in $(find doc/html -name "*.html"); do
                content=$(sed -n '/<div class=\"footdef\"/,/<\/div>/p' $file | sed 's/\\/\\\\/g' | \
                    awk 'BEGIN{j=0}{i=1;while (i<=NF) {line=line" "$i;if (match($i,"</div>")) {array[j]=line;j++;line=""} i++}}END{for (i=0;i<j;i++) print array[i]}')
                sed -i -e '/<div class=\"footdef\"/,/<\/div>/d' $file
                IFS=$'\n'
                i=1
                pkgtools::msg_devel "footnote content=${content}"
                for f in ${=content}
                do
                    pkgtools::msg_devel "footnote #$i=$f"
                    fn=${f:0:-6}
                    fn+="<br/>__div_or_space__\n</p>"
                    awk -v fn=$fn '/<sup><a id="fnr.'$i'"/{a++;}/^<\/p>/&&a{$0=fn;a=0;}1' $file > $file.$i
                    mv $file.$i $file
                    let i=i+1
                done
                unset IFS
                sed -i -e '/__div_or_space__/{n;s@<div class=\"footdef\">@@;s@__div_or_space__@</div>@;}' $file
                tac $file | sed -e '/<sup>/{n;s@__div_or_space__@@;}' | tac > $file.tac
                mv $file.tac $file
                sed -i -e 's@__div_or_space__@</div>@g' $file
            done
        fi

        if [ ${color_scheme[0,1]} = "#" ]; then
            if [ ${#color_scheme} -eq 7 ]; then
                sed -i -e 's/#67ad00/'${color_scheme}'/' doc/html/css/styles.css
            else
                pkgtools::msg_error "The HEX code is not allowed !"
            fi
        elif [ ${__ogp_color_map[${color_scheme}]+_} ];then
            sed -i -e 's/#67ad00/'${__ogp_color_map[${color_scheme}]}'/' doc/html/css/styles.css
        fi
    fi

    pkgtools::msg_debug "Remove logfiles"
    if ! ${keep_tmp_files}; then
        pkgtools::msg_debug "Remove useless files"
        # find . -regex ".*\.\(pyg\|auxlock\|toc\|out\|fls\|aux\|log\|fdb_latexmk\|tex\)" \
        #     -not -path '*doc*' -not -path '*figures*' -exec rm -f {} \;
        #find . -name "*latex.d*" -exec rm -rf {} \;
        find . -name "*~" -exec rm -rf {} \;
        rm -rf ./latex.d
        for file in $(find . -name "*.org"); do
            for ffile in $(\ls -1 ${file/org/}*); do
                if [ ${ffile:e} != "org" ]; then
                    rm $ffile
                fi
            done
        done
    fi

    pkgtools::msg_debug "Remove tmp README file"
    if [ ! -s README.org ]; then
        rm -f README.org
    fi

    pkgtools::at_function_exit
    return 0
}

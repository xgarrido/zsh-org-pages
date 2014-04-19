# -*- mode: shell-script; -*-
#
# Copyright (C) 2014 Xavier Garrido
#
# Author: xavier.garrido@gmail.com
# Keywords: org, html
# Requirements: pkgtools
# Status: not intended to be distributed yet

function org-pages ()
{
    __pkgtools__default_values
    __pkgtools__at_function_enter org-pages

    local append_list_of_options_arg
    local append_list_of_cmd_arg
    local generate_pdf=0
    local generate_html=1
    local clean=0
    local generate=0
    local publish=0
    local recursive=0
    local keep_tmp_files=0
    local generate_floating_footnote=true
    while [ -n "$1" ]; do
        local token="$1"
        if [ "${token[0,1]}" = "-" ]; then
            local opt=${token}
            if [ "${opt}" = "-h" -o "${opt}" = "--help" ]; then
                return 0
            elif [ "${opt}" = "-d" -o "${opt}" = "--debug" ]; then
                pkgtools__msg_using_debug
            elif [ "${opt}" = "-D" -o "${opt}" = "--devel" ]; then
                pkgtools__msg_using_devel
            elif [ "${opt}" = "-v" -o "${opt}" = "--verbose" ]; then
                pkgtools__msg_using_verbose
            elif [ "${opt}" = "--recursive" ]; then
                recursive=1
            elif [ "${opt}" = "--pdf" ]; then
                generate_pdf=1
                generate_html=0
            elif [ "${opt}" = "--html" ]; then
                generate_html=1
                generate_pdf=0
            elif [ "${opt}" = "--floating-footnote" ]; then
                generate_floating_footnote=true
            elif [ "${opt}" = "--keep-tmp-files" ]; then
                keep_tmp_files=1
            else
                append_list_of_options_arg+="${opt} "
            fi
        else
            if [ "${token}" = "clean" ]; then
                clean=1
            elif [ "${token}" = "generate" ]; then
                generate=1
            elif [ "${token}" = "publish" ]; then
                publish=1
                generate=1
                generate_html=1
                generate_pdf=0
            elif [ "x${token}" != "x" ]; then
                append_list_of_cmd_arg+="${token} "
            fi
        fi
        shift
    done

    append_list_of_cmd_arg=$(echo ${append_list_of_cmd_arg} | sed 's/:/:\"/g')
    append_list_of_options_arg=$(echo ${append_list_of_options_arg} | sed 's/=/=\"/g')

    pkgtools__msg_devel "generate=${generate}"
    pkgtools__msg_devel " |- pdf=${generate_pdf}"
    pkgtools__msg_devel " |- html=${generate_html}"
    pkgtools__msg_devel "deploy=${deploy}"
    pkgtools__msg_devel "append_list_of_cmd_arg=${append_list_of_cmd_arg}"
    pkgtools__msg_devel "append_list_of_options_arg=${append_list_of_options_arg}"

    if [ ${clean} -eq 1 ]; then
        find . -name doc -exec rm -rf {} \;
        __pkgtools__at_function_exit
        return 0
    fi

    if [ ${generate} -eq 0 ]; then
        pkgtools__msg_error "No output file will be generated !"
        __pkgtools__at_function_exit
        return 1
    fi

    pkgtools__msg_notice "Start export process..."

    op::prepare_process
    op::process
    op::post_process

    pkgtools__msg_notice "Export successfully done"

    if [ ${publish} -eq 1 ]; then
        pkgtools__msg_notice "Publishing to the web"
	find doc -name *.*~ -exec rm -f {} \;
	(cd doc/html && tar czvf /tmp/org-publish.tar.gz .)
        rm -rf doc
        current_branch_name=$(git branch | grep '*' | awk '{print $2}')
	git checkout gh-pages
	tar xzvf /tmp/org-publish.tar.gz
	if [ -n "`git status --porcelain`" ]; then git commit -am "update doc" && git push; fi
	git checkout ${current_branch_name}
    fi

    unset generate_pdf generate_html
    __pkgtools__at_function_exit
    return 0
}

function op::prepare_process()
{
    __pkgtools__at_function_enter op::prepare_process
    if [ ${generate_html} -eq 1 ]; then
        pkgtools__msg_debug "Parsing org files..."
        for file in $(find . -name "*.org"); do
            \cp $file $file.save
            sed -i -e "s/#+BEGIN_SRC latex/#+BEGIN_SRC latex :results drawer :exports results/g" $file
        done
    fi
    __pkgtools__at_function_exit
    return 0
}

function op::process()
{
    __pkgtools__at_function_enter process
    export OGP_EXPORT_DIR=$PWD

    local ogp_path="${ADOTDIR}/repos/https-COLON--SLASH--SLASH-github.com-SLASH-xgarrido-SLASH-zsh-org-pages.git"
    local emacs_cmd=""
    local emacs_base_cmd="emacs --batch --no-init-file "
    emacs_base_cmd+="--eval \"(require 'org)\" "
    emacs_base_cmd+="--eval \"(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))\" "
    emacs_base_cmd+="--eval \"(setq org-confirm-babel-evaluate nil)\" "
    emacs_base_cmd+="--eval '(let ((this-directory \""$PWD"/\")) "
    emacs_base_cmd+="(org-babel-tangle-file \""${ogp_path}"/zsh-org-pages.org\") "
    emacs_base_cmd+="(org-babel-load-file \""${ogp_path}"/zsh-org-pages.org\"))' "

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
        emacs_cmd+=${emacs_base_cmd}
        if [ ${generate_html} -eq 1 ]; then
            pkgtools__msg_notice "Exporting pages to html..."
            if [ ${recursive} -eq 1 ]; then
                emacs_cmd+="--funcall org-publish-html-recursive "
            else
                emacs_cmd+="--funcall org-publish-html "
            fi
        elif [ ${generate_pdf} -eq 1 ]; then
            pkgtools__msg_notice "Exporting pages to pdf (through latex)..."
            emacs_cmd="TEXINPUTS=\""$PWD"/doc/pdf:\$TEXINPUTS\" "
            emacs_cmd+=${emacs_base_cmd}" "
            emacs_cmd+="--funcall org-publish-pdf "
        fi
        emacs_cmd+="--visit \"README.org\" "
    fi

    pkgtools__msg_debug ${emacs_cmd}
    if [ ${__pkgtools__msg_debug} -eq 1 ]; then
        echo $emacs_cmd | sh
    else
        echo $emacs_cmd | sh > /dev/null 2>&1
    fi
    if $(pkgtools__last_command_fails); then
        pkgtools__msg_error "Export has failed !"
        __pkgtools__at_function_exit
        return 1
    fi

    unset emacs_base_cmd emacs_cmd
    unset ogp_path
    __pkgtools__at_function_exit
    return 0
}

function op::post_process()
{
    __pkgtools__at_function_enter op::post_process
    if [ ${generate_html} -eq 1 ]; then
        pkgtools__msg_notice "Change directory hierarchy for css files"
        for file in $(find doc/html -name "*.html"); do
            count="${file//[^\/]}"
            rel_path=
            for ((i=2;i<${#count};i++));do
                rel_path+="../"
            done
            sed -i -e 's@href="css/@href="'${rel_path}'css/@g' $file
            sed -i \
                -e 's@ding{192}@\(\\unicode{x2460}\\)@g' \
	        -e 's@ding{193}@\(\\unicode{x2461}\\)@g' \
	        -e 's@ding{194}@\(\\unicode{x2462}\\)@g' \
	        -e 's@ding{195}@\(\\unicode{x2463}\\)@g' \
	        -e 's@cmark@\(\\unicode{x2713}\\)@g' \
	        -e 's@xmark@\(\\unicode{x2717}\\)@g' \
                $file
        done
        pkgtools__msg_notice "Exporting pdf figures"
        mkdir -p doc/html/figures
        for img in $(find . -name "*.pdf" -path "*figures*" -not -path "*doc*"); do
            pkgtools__msg_debug "Converting ${img}..."
            convert -density 100 $img doc/html/figures/$(basename ${img/.pdf/.png})
        done
        find . -regex ".*\.\(jpg\|jpeg\|png\|gif\|svg\)" \
            -path "*figures*" -not -path "*doc*" -exec cp {} doc/html/figures/. \;
        pkgtools__msg_debug "Parsing org files..."
        for file in $(find . -name "*.org"); do
            if [ -f $file.save ]; then
                \mv $file.save $file
                sed -i -e "s/#+BEGIN_SRC latex :results drawer :exports results/#+BEGIN_SRC latex/g" $file
            fi
        done

        if ${generate_floating_footnote}; then
            pkgtools__msg_notice "Generate floating footnotes"
            pkgtools__msg_debug "Adding special CSS code"
            sed -i -e "\$a.footdef{\nfont-size: 10px;\nright: 0;\nposition: absolute;\nwidth:180px;\n}" doc/html/css/styles.css
            for file in $(find doc/html -name "*.html"); do
                content=$(sed -n '/<div class=\"footdef\"/,/<\/div>/p' $file | sed 's/\\/\\\\/g' | \
                    awk 'BEGIN{j=0}{i=1;while (i<=NF) {line=line" "$i;if (match($i,"</div>")) {array[j]=line;j++;line=""} i++}}END{for (i=0;i<j;i++) print array[i]}')
                sed -i -e '/<div class=\"footdef\"/,/<\/div>/d' $file
                IFS=$'\n'
                i=1
                pkgtools__msg_debug "content=${content}"
                for f in ${content}
                do
                    awk -v fn="$f" '/<sup><a id="fnr.'$i'"/{a++;}/<\/p>/&&a{$0=fn;a=0;}1' $file > $file.$i
                    mv $file.$i $file
                    let i=i+1
                done
                unset IFS
            done
        fi
    fi

    if [ ${keep_tmp_files} -eq 0 ]; then
        pkgtools__msg_debug "Remove useless files"
        find . -regex ".*\.\(pyg\|auxlock\|toc\|out\|fls\|aux\|log\|fdb_latexmk\|tex\)" \
            -not -path '*doc*' -not -path '*figures*' -exec rm -f {} \;
        find . -name "*~" -exec rm -rf {} \;
        #find . -name "*latex.d*" -exec rm -rf {} \;
        rm -rf ./latex.d
    fi
    __pkgtools__at_function_exit
    return 0
}

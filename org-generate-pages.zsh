# -*- mode: shell-script; -*-
#
# Copyright (C) 2014 Xavier Garrido
#
# Author: xavier.garrido@gmail.com
# Keywords: org, html
# Requirements: pkgtools
# Status: not intended to be distributed yet

function org_generate_pages ()
{
    __pkgtools__default_values
    __pkgtools__at_function_enter org_generate_pages

    local append_list_of_options_arg
    local append_list_of_cmd_arg
    local generate_pdf_doc=0
    local generate_html_doc=0
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
            elif [ "${opt}" = "--pdf" ]; then
                generate_pdf_doc=1
            else
                if [[ "${opt}" == *=* || "${opt}" == *:* ]]; then
                    append_list_of_options_arg+="${opt}\" "
                else
                    append_list_of_options_arg+="${opt} "
                fi
            fi
        else
            if [ "x${token}" != "x" ]; then
                if [[ "${token}" == *=* || "${token}" == *:* ]]; then
                    append_list_of_cmd_arg+="${token}\" "
                else
                    append_list_of_cmd_arg+="${token} "
                fi
            fi
        fi
        shift
    done

    append_list_of_cmd_arg=$(echo ${append_list_of_cmd_arg} | sed 's/:/:\"/g')
    append_list_of_options_arg=$(echo ${append_list_of_options_arg} | sed 's/=/=\"/g')

    pkgtools__msg_devel "append_list_of_cmd_arg=${append_list_of_cmd_arg}"
    pkgtools__msg_devel "append_list_of_options_arg=${append_list_of_options_arg}"

    ogp_path="/home/garrido/Development/org-generate-pages"
    emacs_cmd+="emacs --batch --no-init-file "
    emacs_cmd+="--eval \"(require 'org)\" "
    emacs_cmd+="--eval \"(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))\" "
    emacs_cmd+="--eval \"(setq org-confirm-babel-evaluate nil)\" "
    emacs_cmd+="--eval '(let ((this-directory \""$PWD"/\")) "
    emacs_cmd+="(org-babel-tangle-file \""${ogp_path}"/org-generate-pages.org\") "
    emacs_cmd+="(org-babel-load-file \""${ogp_path}"/org-generate-pages.org\"))' "
    emacs_cmd+="--visit \"README.org\" --funcall org-publish-html"

    echo $emacs_cmd | sh
    unset emacs_cmd
    unset ogp_path
    __pkgtools__at_function_exit
    return 0
}

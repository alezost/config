#!/usr/bin/guile \
-e main -s
!#
;;; config.scm --- Deploy config files

;; Copyright © 2015, 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  3 Mar 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script may be used to list, show info and deploy (create
;; symlinks) various configuration files.

;;; Code:

(use-modules
 (ice-9 format)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-37)
 (al places)
 (al files)
 (al messages)
 (al links)
 (al configs))

(setlocale LC_ALL "")


;;; Configs

(define %configs
  (list
   (config* #:name "config"
            #:links (list
                     (link* #:filename (bin-file "config")
                            #:target (config-file "config.scm"))))
   (config* #:name "guix"
            #:links (list
                     (link* #:filename (bin-file "profile")
                            #:target (guix-script-file "profile.scm"))
                     (link* #:filename (bin-file "system")
                            #:target (guix-script-file "system.scm"))))
   (config* #:name "shepherd"
            #:links (list
                     (link* #:filename (home-file ".config/shepherd/init.scm")
                            #:target (config-file "shepherd/init.scm"))))
   (config* #:name "emacs"
            #:links (list
                     (link* #:filename (home-file ".emacs.d/init.el")
                            #:target (config-file "emacs/config/init.el"))))
   (config* #:name "stumpwm"
            #:links (list
                     (link* #:filename (home-file ".stumpwmrc")
                            #:target (config-file "stumpwm/init.lisp"))))
   (config* #:name "conkeror"
            #:links (list
                     (link* #:filename (home-file ".conkerorrc")
                            #:target (config-file "conkeror/init.js"))))

   (config* #:name "ssh"
            #:links (list
                     (link* #:filename (home-file ".ssh")
                            #:target (config-file "ssh"))))
   (config* #:name "gpg"
            #:links (list
                     (link* #:filename (home-file ".gnupg")
                            #:target (config-file "gpg"))))
   (config* #:name "auth"
            #:links (list
                     (link* #:filename (home-file ".authinfo.gpg")
                            #:target (config-file "auth/authinfo.gpg"))))
   (config* #:name "top"
            #:links (list
                     (link* #:filename (home-file ".toprc")
                            #:target (config-file "top/toprc"))))
   (config* #:name "sbcl"
            #:links (list
                     (link* #:filename (home-file ".sbclrc")
                            #:target (config-file "sbcl/sbclrc"))))
   (config* #:name "git"
            #:links (list
                     (link* #:filename (home-file ".gitconfig")
                            #:target (config-file "git/gitconfig"))))
   (config* #:name "lirc"
            #:links (list
                     (link* #:filename (home-file ".lircrc")
                            #:target (config-file "lirc/lircrc"))))
   (config* #:name "mime"
            #:links (list
                     (link* #:filename (home-file ".mime.types")
                            #:target (config-file "mime/mime.types"))))
   (config* #:name "rtorrent"
            #:links (list
                     (link* #:filename (home-file ".rtorrent.rc")
                            #:target (config-file "rtorrent/rc"))))
   (config* #:name "tvtime"
            #:links (list
                     (link* #:filename (home-file ".tvtime")
                            #:target (config-file "tvtime"))))
   (config* #:name "mplayer"
            #:links (list
                     (link* #:filename (home-file ".mplayer")
                            #:target (config-file "mplayer"))))

   (config* #:name "mpv"
            #:links (list
                     (link* #:filename (home-file ".config/mpv")
                            #:target (config-file "mpv"))))
   (config* #:name "openbox"
            #:links (list
                     (link* #:filename (home-file ".config/openbox")
                            #:target (config-file "openbox"))))
   (config* #:name "dunst"
            #:links (list
                     (link* #:filename (home-file ".config/dunst")
                            #:target (config-file "dunst"))))
   (config* #:name "zathura"
            #:links (list
                     (link* #:filename (home-file ".config/zathura")
                            #:target (config-file "zathura"))))

   (config* #:name "X"
            #:links (list
                     (link* #:filename (home-file ".Xmodmap")
                            #:target (config-file "X/Xmodmap"))
                     (link* #:filename (home-file ".Xresources")
                            #:target (config-file "X/Xresources"))
                     (link* #:filename (home-file "XTerm")
                            #:target (config-file "X/XTerm"))))

   (config* #:name "bash"
            #:links (list
                     (link* #:filename (home-file ".inputrc")
                            #:target (config-file "bash/inputrc"))
                     (link* #:filename (home-file ".bashrc")
                            #:target (config-file "bash/bashrc"))
                     (link* #:filename (home-file ".bash_profile")
                            #:target (config-file "bash/profile"))))))

(define (configs-names)
  "Return list of all available config names."
  (map config-name %configs))

(define (configs-links)
  "Return list of all available config links."
  (append-map config-links %configs))


;;; Command-line args

(define (show-help)
  (display "Usage: config OPTION [CONFIG]...
List, show info or deploy available/specified configurations.")
  (display "\n
Options:
  -h, --help        display this help and exit
  -l, --list        list available configurations and exit
  -O, --list-old    list old configuration files and exit
  -o, --ls-old      perform 'ls -l' on the old configuration files and exit
  -D, --delete-old  delete old configuration files and exit
  -s, --show        show info of the specified configurations
  -d, --deploy      deploy (create symlinks) the specified configurations")
  (display "\n
If '--show' or '--deploy' option is used and no configuration is
specified, then all available ones will be shown or deployed.")
  (newline))

(define* (list-strings strings #:key title
                       (proc (lambda (s)
                               (format #t "~{~a~%~}" (sort s string-ci<)))))
  "Display list of STRINGS using PROC."
  (when title
    (display title)
    (newline))
  (proc strings))

(define (show-configs-names)
  (list-strings (configs-names)
                #:title "Available configurations:"))

(define (call-with-old-files proc)
  "Call PROC on old configuration files."
  (let ((files (old-files)))
    (if (null? files)
        (display "There are no old configuration files.\n")
        (proc (old-files)))))

(define (show-old-files)
  (call-with-old-files
   (lambda (files)
     (list-strings files
                   #:title "Old configuration files:"))))

(define (ls-old-files)
  "Perform 'ls -l' on the old configuration files."
  (call-with-old-files
   (lambda (files)
     (list-strings
      files
      #:title "Old configuration files:"
      #:proc (lambda (files)
                   (apply system*
                          "ls" "-l" "--directory" "--color=auto"
                          (sort files string-ci<)))))))

(define (delete-old-files)
  (call-with-old-files (cut map delete-file-recursively <>)))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\l "list") #f #f
                (lambda _
                  (show-configs-names)
                  (exit 0)))
        (option '(#\O "list-old") #f #f
                (lambda _
                  (show-old-files)
                  (exit 0)))
        (option '(#\o "ls-old") #f #f
                (lambda _
                  (ls-old-files)
                  (exit 0)))
        (option '(#\D "delete-old") #f #f
                (lambda _
                  (delete-old-files)
                  (exit 0)))
        (option '(#\s "show") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action show-config seed)))
        (option '(#\d "deploy") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action
                              (cut deploy-config <> old-unique-filename)
                              seed)))))

(define (parse-args args)
  "Return alist of options from command-line ARGS."
  (reverse
   (args-fold args %options
              (lambda (opt name arg seed)
                ((message-proc #:destination (current-error-port))
                 "Unrecognized option: '~a'." name)
                seed)
              (lambda (arg seed)
                (alist-cons 'config arg seed))
              '())))



(define (old-filename filename)
  "Return name of an old config file that should be backed up."
  (string-append filename "-old"))

(define (old-unique-filename filename)
  "Return unique name of an old config file that should be backed up."
  (unique-filename (old-filename filename)))

(define (old-files)
  "Return list of old config files."
  (append-map (lambda (link)
                (find-matching-files
                 (old-filename (link-filename link))))
              (configs-links)))

(define (options->configs-names opts)
  "Return list of config names from OPTS alist."
  (filter-map (match-lambda
               (('config . name) name)
               (_ #f))
              opts))

(define (lookup-config name)
  "Return config record from '%configs' list by its NAME."
  (find (lambda (config)
          (equal? (config-name config) name))
        %configs))

(define (lookup-configs names)
  "Return config records from '%configs' list by their NAMES."
  (filter-map
   (lambda (name)
     (or (lookup-config name)
         (begin ((message-proc #:destination (current-error-port))
                 "No '~a' configuration was found." name)
                #f)))
   names))

(define (main args)
  (match args
    ((command args ...)
     (let* ((opts    (parse-args args))
            (action  (or (assq-ref opts 'action)
                         (leave (string-append
                                 "No action is specified.~%"
                                 "Try '~a --help' for more information.")
                                command)))
            (names   (options->configs-names opts))
            (configs (if (null? names)
                         %configs
                         (lookup-configs names))))
       (map action configs)))))

;;; config.scm ends here

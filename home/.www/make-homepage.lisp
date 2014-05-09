#!/usr/bin/env cl

(ql:quickload '(:cl-yaml :anaphora :cl-markup))
(use-package :anaphora)
(use-package :markup)

(defparameter +path+
  (merge-pathnames #p".www/" (user-homedir-pathname)))

(defparameter +bookmarks-path+
  (merge-pathnames #p"self/bookmarks.yaml"
                   (user-homedir-pathname)))
(defparameter +style-path+
  (merge-pathnames #p"style.css" +path+))
(defparameter +js-path+
  (merge-pathnames #p"logs.js" +path+))
(defparameter +homepage-path+
  (merge-pathnames #p"homepage.html" +path+))

(defparameter +bookmarks+ (yaml:parse +bookmarks-path+))

(defun step-node (node)
  (aif (gethash "sub" node)
       ;; Folder
       (markup (:span (gethash "label" node))
               (:ul :class "folder"
                (loop for sub-node in it do
                  (step-node sub-node))))
       ;; Bookmark
       (markup (:li (:a :href (gethash "uri" node)
                        (gethash "label" node))))))

(with-open-file (*output-stream*
                 +homepage-path+
                 :direction :output
                 :if-does-not-exist :create
                 :if-exists :supersede)
  (html5
   (:head (:title "Bookmarks")
          (:meta :charset "utf-8")
          (:link :rel "stylesheet"
                 :href +style-path+))
   (:body
    (:div :id "bar"
          (:ul
           (:li (:a :href "https://github.com/" "gh"))
           (:li (:a :href "https://twitter.com/" "tw"))
           (:li (:a :href "https://www.facebook.com/" "fb"))
           (:li (:a :href "https://mail.google.com/mail/u/0/#inbox"
                    "mail"))
           (:li (:a :href "https://mail.google.com/mail/u/1/#inbox"
                    "work mail"))
           (:li (:a :href "https://tryo.slack.com/messages" "slack"))
           (:li (:a :href "http://www.reddit.com/" "reddit"))))
    (:div :id "links" :class "column"
          (:div :id "logs"
                (:h3 "Logs")
                (:ul
                 (:li (:a :id "yesterday" "Yesterday's logs"))
                 (:li (:a :id "today" "Today's logs"))))
          (:div :id "ports"
                (:h3 "Ports")
                (:ul
                 (:li (:a :href "http://localhost:8000/" "8000"))
                 (:li (:a :href "http://localhost:8080/" "8080"))
                 (:li (:a :href "http://localhost:9000/" "9000")))))
    (:div :id "bookmarks" :class "column"
          (:h3 "Bookmarks")
          (loop for node in +bookmarks+ do
            (step-node node)))

    (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
             "")
    (:script :src +js-path+ ""))))

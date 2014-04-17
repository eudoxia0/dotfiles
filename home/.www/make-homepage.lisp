#!/usr/bin/env cl

(ql:quickload '(:cl-yaml :anaphora :cl-markup))
(use-package :anaphora)
(use-package :markup)

(defparameter +path+
  (merge-pathnames
   #p".www/"
   (user-homedir-pathname)))

(defparameter +bookmarks-path+
  (merge-pathnames #p"bookmarks.yaml" +path+))
(defparameter +style-path+
  (merge-pathnames #p"style.css" +path+))
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

(with-open-file (*output-stream* +homepage-path+
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
  (html5
   (:head (:title "Bookmarks")
          (:meta :charset "utf-8")
          (:link :rel "stylesheet"
                 :href +style-path+))
   (:body
    (:section :id "bar"
              (:ul
                (:li (:a :href "https://github.com/"
                         "gh"))
                (:li (:a :href "https://twitter.com/"
                         "tw"))))
    (:section :id "ports"
              (:ul
                (:li (:a :href "http://localhost:8000/"
                         "8000"))
                (:li (:a :href "http://localhost:8080/"
                         "8080"))
                (:li (:a :href "http://localhost:9000/"
                         "9000"))))
    (:section :id "bookmarks"
              (loop for node in +bookmarks+ do
                (step-node node))))))

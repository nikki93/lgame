;;;; lgame.asd

(asdf:defsystem #:lgame
  :description "Describe lgame here"
  :author "Nikhilesh Sigatapu <s.nikhilesh@gmail.com>"
  :license "MIT"
  :depends-on (#:lispbuilder-sdl
               #:sdl2)
  :serial t
  :components ((:file "package")
               (:file "lgame")))


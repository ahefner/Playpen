(asdf:defsystem :playpen
  :name "Playpen"
  :description "It's my ball and you can't play with it."
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi
               :bordeaux-threads
               :cl-opengl :cl-glu
               :skippy
               ;; :zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa
               )
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "syms")
                             (:file "windowing")
                             (:file "generics")
                             (:file "images")
                             (:file "image-loader")
                             (:file "graphics")
                             (:file "ui")
                             (:module tests
                              :serial t
                              :components
                              ((:file "image-loader-tests")
                               (:file "multiwindow-test")
                               (:file "animation-test-1")
                               (:file "photos")
                               (:file "clock")
                               ;; These two are probably bit-rotted:
                               #+NIL (:file "window-test-1")
                               #+NIL (:file "window-test-2")))))))


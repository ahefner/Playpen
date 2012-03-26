(in-package :playpen)

(cffi:defctype ft-face :pointer)

(cffi:defcfun text-init :boolean)

(cffi:defcfun text-load-face ft-face
  (filename :pointer)
  (facenum  :int))

(cffi:defcstruct rendered-glyph
  (index :unsigned-int)
  (width :int)
  (height :int)
  (bitmap-left :int)
  (bitmap-top :int)
  (pitch :int)
  (advance-x :int)
  (advance-y :int)
  (f-advance-x :float)
  (f-advance-y :float)
  (buffer :pointer))

(cffi:defcfun text-render-glyph (:pointer rendered-glyph)
  (face ft-face)
  (text-height :unsigned-int)
  (index :unsigned-int)
  (convert-to-rgba :int))

;;; Quick hack..
(defun test-face-filename ()
  (builtin-asset-path
   #+NIL "fonts/Roboto_Hinted_20111129/Roboto-Thin.ttf"
   #+NIL "fonts/Droid/DroidSans.ttf"
   #+NIL "fonts/dejavu-fonts-ttf-2.33/ttf/DejaVuSans.ttf"
   "fonts/GentiumPlus-1.508/GentiumPlus-R.ttf"
   #+NIL "fonts/dejavu-fonts-ttf-2.33/ttf/DejaVuSerif.ttf"
   #+NIL "fonts/Roboto_Hinted_20111129/Roboto-Medium.ttf"))

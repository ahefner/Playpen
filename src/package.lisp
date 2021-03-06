;; -*- Mode: Lisp; -*-

(in-package #:cl-user)


(defpackage #:putil
  (:use #:cl)
  (:export

   ;;; General utilities:

   #:textual-condition

   #:keywordify #:suffix
   #:non-null
   #:clamp #:birand #:extend-shift

   #:orf #:minf #:maxf
   #:2pi
   #:min-or
   #:max-or
   #:scalec

   #:deftuple

   #:float->byte #:byte->float #:component* #:rgba-avg #:rgba-avg-quad
   #:rescale-component #:xy->row-major-index
   #:process-components-rgba #:loop-rectangle

   #:filename
   #:filename-vector
   #:human-string
   #:machine-string
   #:with-filename-pointer

   #:p<<=
   #:p>>=
   #:rectangle-contains-p

   #:with-array-pointer

   ;;; C stuff

   #:size_t
   #:off_t
   #:malloc
   #:free
   #:calloc
   #:memset

   ;;; OS interface and utilities:
   #:fd-write-byte
   #:fd-read-byte

   ))

(defpackage #:pwin
  (:use #:cl #:putil)
  (:export
   #:opengl-context
   #:resource-map
   #:*global-opengl-context*
   #:*gl-context*
   #:assert-gl-context
   #:asserted-gl-context
   #:opengl-resource
   #:assert-ownership
   #:attach-resource
   #:remove-resource
   #:context-attributes
   #:context-attribute

   #:initialize-display
   #:*display*
   #:display-width
   #:display-height
   #:display-supports-compositing-p
   #:display-dpi

   #:window
   #:window-width
   #:window-height
   #:window-user-pointer
   #:window-pointer-position
   #:window-button-state
   #:window-lock
   #:graphics-context

   #:initial-width
   #:initial-height
   #:application-name
   #:title
   #:window-type

   #:create-window
   #:destroy-window
   #:scribble-window
   #:set-title
   #:all-windows
   #:destroy-all-windows
   #:number-of-windows
   #:window-alive-p
   #:window-composited-p

   #:begin-paint
   #:end-paint

   #:event
   #:window-event
   #:motion
   #:pointer-enter
   #:pointer-exit
   #:button-press
   #:button-release
   #:key-press
   #:key-release
   #:mapped
   #:unmapped
   #:close-request
   #:expose
   #:resized
   #:io-ready
   #:timeout

   #:event-type
   #:event-window
   #:event-x
   #:event-y
   #:event-button-state
   #:event-old-button-state
   #:event-button
   #:event-modifier-mask
   #:event-native-code
   #:event-native-keysym
   #:event-keysym
   #:event-unicode
   #:event-width
   #:event-height
   #:event-old-width
   #:event-old-height

   #:+pointer-button-1+
   #:+pointer-button-2+
   #:+pointer-button-3+
   #:+pointer-wheel-up+
   #:+pointer-wheel-down+
   #:+pointer-wheel-left+
   #:+pointer-wheel-right+

   #:reset-watched-fds
   #:watch-fd
   #:check-fd
   #:get-event
   #:usectime))

(defpackage #:playpen
  (:use #:cl #:putil #:pwin)
  (:export

   ;; Re-export event/windowing symbols from PWIN package:
   #:*gl-context*
   #:display-width
   #:display-height
   #:display-supports-compositing-p
   #:display-dpi

   #:event
   #:window-event
   #:motion
   #:pointer-event
   #:pointer-enter
   #:pointer-exit
   #:button-press
   #:button-release
   #:key-event
   #:key-press
   #:key-release
   #:mapped
   #:unmapped
   #:close-request
   #:expose
   #:resized
   #:timeout

   #:event-type
   #:event-window
   #:event-x
   #:event-y
   #:event-button-state
   #:event-old-button-state
   #:event-button
   #:event-modifier-mask
   #:event-native-code
   #:event-native-keysym
   #:event-keysym
   #:event-unicode
   #:event-width
   #:event-height
   #:event-old-width
   #:event-old-height

   #:+pointer-button-1+
   #:+pointer-button-2+
   #:+pointer-button-3+
   #:+pointer-wheel-up+
   #:+pointer-wheel-down+
   #:+pointer-wheel-left+
   #:+pointer-wheel-right+

   ;; Windows
   #:window
   #:window-width
   #:window-height
   #:window-pointer-position
   #:window-button-state
   #:window-lock
   #:window-composited-p

   #:initial-width
   #:initial-height
   #:application-name
   #:title
   #:set-title
   #:window-type
   #:create-window
   #:destroy-window

   ;; (end of PWIN symbols)

   ;;; Misc. generic functions
   #:width
   #:height
   #:dimensions
   #:area
   #:aspect-ratio
   #:coordinate

   ;;; Concurrency
   #:send
   #:receive
   #:channel-error
   #:channel-empty
   #:channel-closed
   #:transact
   #:drain
   #:channel-open-p

   ;;; Pixel format protocol
   #:pixel-format-element-type
   #:decode-pixel-float
   #:decode-pixel-integer
   #:pixel-red-f
   #:pixel-green-f
   #:pixel-blue-f
   #:pixel-alpha-f
   #:pixel-red-i
   #:pixel-green-i
   #:pixel-blue-i
   #:pixel-alpha-i

   ;;; Bitfield pixel formats
   #:bf-format
   #:bf-format-red-position
   #:bf-format-red-size
   #:bf-format-red-bias
   #:bf-format-green-position
   #:bf-format-green-size
   #:bf-format-green-bias
   #:bf-format-blue-position
   #:bf-format-blue-size
   #:bf-format-blue-bias
   #:bf-format-alpha-position
   #:bf-format-alpha-size
   #:bf-format-alpha-bias
   #:intern-bf-format
   #:rgb
   #:rgba
   #:+rgb+
   #:+rgba+
   #:+alpha+

   ;;; Image protocol
   #:pixel-format
   #:image-pitch
   #:image-offset
   #:image-row-major-offset
   #:image-row-major-offset-bytes
   #:data-array

   ;;; Concrete image types
   #:image-vector
   #:make-image-vector
   #:image-vector-data
   #:image-vector-offset
   #:image-vector-width
   #:image-vector-pitch
   #:image-vector-height
   #:image-vector-pixel-format
   #:image-matrix
   #:make-image-matrix
   #:image-matrix-data
   #:image-matrix-pixel-format

   ;;; Surfaces
   ;#:cache-surface
   ;#:cache-surface-allocate
   ;#:mirror-push-rectangle
   ;#:mirror-pull-rectangle
   ;#:width
   ;#:height
   ;#:mirrored-surface
   ;#:bounded-surface
   ;#:source-surface

   ;;; Images
   #:read-jpeg-file
   #:jpeg-error
   #:jpeg-decoder-error
   #:jpeg-warning
   #:jlo-mode-fastest
   #:jlo-mode-high-quality
   #:jlo-set-downscale

   #:read-gif-file

   #:read-png-file
   #:png-error

   #:read-image-file

   ;;; Drawing
   #:call-with-graphics-context
   #:with-graphics-context
   #:*window*

   #:reset-transforms
   #:use-pixel-projection
   #:use-centered-pixel-projection
   #:use-graphic-projection
   #:use-texture
   #:clear-screen

   #:texture-update-rectangle

   #:transform-to-screen
   #:transform-from-screen
   #:transform-to-screen*

   ;; Applications
   #:handle-event
   #:animating
   #:wakeup-time
   #:event-loop-hook
   #:run-app
   #:start-event-loop

   #:builtin-asset-path
   #:image-asset

   ;; Animation
   #:time-consumer                      ; temporary; should include by default.
   #:animate
   #:relative-time
   #:delta-t
   #:expt-approach

   ;;; Fonts and text
   ;#:text-face
))


(defpackage #:pwin-tests
  (:use #:cl #:pwin #:putil)
  (:export #:run-test-1 #:run-test-2))


(defpackage #:playpen-tests
  (:use #:cl #:playpen #:putil)
  (:export
   #:multiwindow-test
   #:animation-test-1
   #:photos
   #:clock))

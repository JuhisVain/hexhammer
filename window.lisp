(in-package :hexhammer)

(defun test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window
	(window :title "HH" :w 1000 :h 800 :flags '(:shown :resizable))
      (sdl2:with-renderer
	  (renderer window :index -1 :flags '(:accelerated :presentvsync))
      
	(let ((texture (sdl2:create-texture
			renderer :ARGB8888 :streaming 1000 800)))
	  (autowrap:with-alloc (buffer :unsigned-char (* 1000 800 4))
	    
	    
	    (sdl2:set-render-draw-color renderer 100 100 100 255)
	    (sdl2:render-clear renderer)
	    (sdl2:render-present renderer)
	    (let* ((cairo-surface
		     (cairo:create-image-surface-for-data
		      buffer :argb32 1000 800 (* 4 1000)))
		   (cairo-context (cairo:create-context cairo-surface)))
	      (cairo:with-context (cairo-context)
		(cairo:set-source-rgb 0 0 0)
		(cairo:paint)
		(cairo:set-source-rgb 0 1 0)
		(cairo:set-line-width 3)
		(cairo:move-to 0 0)
		(cairo:line-to 1000 800)
		(cairo:line-to 500 200)
		(cairo:line-to 0 0)
		(cairo:stroke))
	      (cairo:destroy cairo-context)
	      (cairo:destroy cairo-surface))

	    ;; https://wiki.libsdl.org/MigrationGuide

	    (sdl2:with-event-loop (:method :poll)
	      
              (:idle ()
		     (sdl2:update-texture texture nil
					  buffer
					  (* 4 1000)) ; ARGB8888 size * texture width
		     (sdl2:render-clear renderer)
		     (sdl2:render-copy renderer texture)
		     (sdl2:render-present renderer)
		     )
	      (:quit () t)
	      )
	    
	    ))))))

;(defun render-texture (texture renderer x y)
 ; (sdl2:query-texture texture)
  ;(sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y ))

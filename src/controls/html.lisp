(in-package :xul)

(define-xul-element html-element (xul-element)
  ())

(defmethod element-name ((element html-element))
  (let ((name (call-next-method)))
    (format nil "html:~A" name)))

(define-xul-element a (html-element)
  (href target style))

(define-xul-element h1 (html-element)
  (style))

(define-xul-element div (html-element container-element)
  ())

(define-xul-element p (html-element)
  ())

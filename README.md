CL-XUL
------

[**CL-XUL**](https://github.com/mmontone/cl-xul) is a library that helps to create [**Mozilla XUL**](https://developer.mozilla.org/en/docs/XUL) user interfaces in Common Lisp.

It uses [IOlib](http://www.cliki.net/IOlib) based [clws](http://www.cliki.net/clws) library for the communication between the Mozilla XULRunner and Common Lisp via [web sockets](http://en.wikipedia.org/wiki/WebSocket). Because of the IOlib limitation, it works only on Linux at the moment, until a platform independent implementation of websockets is found. Also, I've only tried it from [SBCL](http://www.sbcl.org) so far.

CL-XUL implements a component-based architecture, standard widgets support, easy communication between client and server and automatic view updates and a readable description of GUI directly in Common Lisp, with no need of external XML files.  

Its implementation can be considered similar to that of the [Phobos](http://code.google.com/p/phobos-framework/) Smalltalk framework,  and to the javascript implementation [XULJet](https://code.google.com/p/xuljet/), although it is not quite the same.

This is still work in progress, and there's no documentation at the moment (it is coming soon, though) but a demo can be tried. For those interested, point the `cl-xul.asd` and `cl-xul-test.asd` system definition files from `./sbcl/system (ln -s <system definition file path>)` and then evaluate:

`(require :cl-xul-test)`

and

`(xul-test:showcase)`

from your lisp listener. 

You will also need to satisfy the system dependencies:

- alexandria
- log5
- parenscript
- cxml
- cl-fad
- closer-mop
- clws
- cl-json
- md5

The easiest way of installing those packages is via [Quicklisp](http://www.quicklisp.org/).

This library is under the MIT licence.

Screenshots
-----------

![Screenshot 1](https://raw.github.com/mmontone/cl-xul/master/doc/images/screenshot1.png)

![Screenshot 2](https://raw.github.com/mmontone/cl-xul/master/doc/images/screenshot2.png)

![Screenshot 3](https://raw.github.com/mmontone/cl-xul/master/doc/images/screenshot3.png)

(in-package :xul)

(define-xul-element menu-bar (container-element)
    (grippyhidden statusbar)
    (:documentation "A container that usually contains menu elements. On the Macintosh, the menubar is displayed along the top of the screen, and all non-menu related elements inside the menubar will be ignored. To look native (at least on Windows), the menubar element needs to be placed inside a toolbox element."))

(define-xul-element menu (container-element)
  (acceltext accesskey allowevents command crop disabled image label menuactive (open* :attribute-name "open") sizetopopup tabindex value)
  (:documentation "An element, much like a button, that is placed on a menubar. When the user clicks the menu element, the child menupopup of the menu will be displayed. This element is also used to create submenus."))

(define-xul-element menu-item (xul-element)
  (acceltext accesskey allowevents autocheck checked closemenu command crop description disabled image key label name selected tabindex (type* :attribute-name "type") validate value)
  (:documentation "A single choice in a menupopup element. It acts much like a button but it is rendered on a menu."))

(define-xul-element menu-popup (container-element)
  (ignorekeys left onpopuphidden onpopuphiding onpopupshowing onpopupshown (position* :attribute-name "position") top)
  (:documentation "A container used to display the contents of a popup menu. When a menupopup is open, it floats above the window and may extend outside the window border. There are several ways in which a menupopup may be used:

    It may be placed inside a menu, menulist, toolbarbutton, or a button with the type attribute set to \"menu\" to create a popup that will open when the menu or button is pressed.
    It may be attached to any element using the popup attribute. When the element is clicked with the left mouse button, the menupopup will be displayed.
    It may be attached to any element using the context attribute. When a context menu is opened, the menupopup will be displayed. A context menu may be opened by right-clicking the element, or by pressing the menu key on the keyboard."))

(define-xul-element menu-separator (xul-element)
  (acceltext accesskey allowevents command crop disabled image label selected tabindex value)
  (:documentation "Used to create a separator between menu items. Typically drawn as a thin line."))

(define-xul-element menu-list (xul-element)
  (accesskey crop disableautoselect disabled editable focused image label oncommand open preference readonly sizetopopup tabindex value)
  (:documentation "An element that can be used for drop-down choice lists. The user may select one of the elements displayed in the menulist. The currently selected choice is displayed on the menulist element. To create the drop-down, put a menupopup inside the menulist containing the choices as menuitem elements. The command event may be used to execute code when the menulist selection changes."))

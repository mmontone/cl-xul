(in-package :xul)

(define-xul-element tab-box (container-element)
  (eventnode handleCtrlPageUpDown handleCtrlTab
	     (selected-index :attribute-name "selectedIndex"))
  (:documentation "A container used to display a set of tabbed pages of elements. A row of tabs is displayed at the top of tabbox which may be used to switch between each page. The tabbox should contain two children, the first a tabs element which contains the tabs and the second a tabpanels element which contains the contents of the pages."))

(define-xul-element tabs (container-element)
  (closebutton disableclose disabled onclosetab onnewtab onselect setfocus selectedIndex tabbox tabindex tooltiptextnew value)
  (:documentation "A row of tabs. A tabs element should be placed inside a tabbox and should contain tab elements.

NB: You can add some other elements to tabs such as button, but they will receive an index. Activating them will not change the selectedIndex."))

(define-xul-element tab (xul-element)
  (accesskey afterselected beforeselected command crop disabled
	     first-tab image label last-tab linkedpanel oncommand
	     pending pinned selected tabindex unread validate value)
  (:documentation "A single tab which should be placed inside a tabs element.
The user may click a tab to bring the associated page of the tabbox to the front."))

(define-xul-element tab-panels (container-element)
  ((selected-index :attribute-name "selectedIndex"))
  (:documentation "A container to hold the set of pages in a tabbox.
The tabpanels element should be placed in a tabbox although it does not have to be a direct child.
The children of the tabpanels element become the panels of the tabbox.
In most cases you would use a vbox, but they can be any element, although some people like to use tabpanel elements.
By clicking the first tab, the first panel will be displayed.
By clicking the second tab, the second panel will be displayed and so on.
There should be the same number of panels as there are tabs.
Panels should never be hidden; hiding the tab suffices to make the panel inaccessible."))

(define-xul-element tab-panel (container-element)
  ()
  (:documentation "A individual panel in a tabpanels element. This element is optional and you may just use any other container in place of it."))

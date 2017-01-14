module Html.Accessibility
    exposing
        ( -- Global states
          CurrentType(..)
        , ariaCurrent
        , ariaKeyshortcuts
        , ariaRoledescription
          -- Widget attributes
        , AutocompleteType(..)
        , ariaAutocomplete
        , ariaChecked
        , Tristate(..)
        , ariaDisabled
        , ariaErrormessage
        , ariaExpanded
        , PopupType(..)
        , ariaHaspopup
        , ariaHidden
        , InvalidityReason(..)
        , ariaInvalid
        , ariaLabel
        , ariaLevel
        , ariaModal
        , ariaMultiline
        , ariaMultiselectable
        , Orientation(..)
        , ariaOrientation
        , ariaPlaceholder
        , ariaPressed
        , ariaReadonly
        , ariaRequired
        , ariaSelected
        , Sort(..)
        , ariaSort
        , ariaValuemax
        , ariaValuemin
        , ariaValuenow
        , ariaValuetext
          -- Live Region Attributes
        , ariaAtomic
        , ariaBusy
        , Live(..)
        , ariaLive
        , Relevant(..)
        , ariaRelevant
          -- Drag-and-Drop Attributes
        , DropEffect(..)
        , ariaDropeffect
        , ariaGrabbed
          -- Relationship Attributes
        , ariaActivedescendant
        , ariaColcount
        , ariaColindex
        , ariaColspan
        , ariaControls
        , ariaDescribedby
        , ariaDetails
        , ariaFlowto
        , ariaLabelledby
        , ariaOwns
        , ariaPosinset
        , ariaRowcount
        , ariaRowindex
        , ariaRowspan
        , ariaSetsize
          -- Helpers
        , boolToTristate
        )

{-| This module provides attributes for improving accessibility as defined by [WAI-ARIA](https://w3c.github.io/aria/aria/aria.html).

# ARIA
## Global States and Properties
Global states and properties are supported by all roles and by all base markup elements.

@docs ariaAtomic, ariaBusy, ariaControls, CurrentType, ariaCurrent, ariaDescribedby, ariaDetails, ariaDisabled, ariaDropeffect, ariaErrormessage, ariaFlowto, ariaGrabbed, ariaHaspopup, ariaHidden, InvalidityReason, ariaInvalid, ariaKeyshortcuts, ariaLabel, ariaLabelledby, Live, ariaLive, ariaOwns, Relevant, ariaRelevant, ariaRoledescription

## Widget Attributes
Attributes specific to common user interface elements found on GUI systems or in rich internet applications which receive user input and process user actions. These attributes are used to support the widget roles.

@docs AutocompleteType, ariaAutocomplete, Tristate, ariaChecked, ariaDisabled, ariaErrormessage, ariaExpanded, PopupType, ariaHaspopup, ariaHidden, ariaInvalid, ariaLabel, ariaLevel, ariaModal, ariaMultiline, ariaMultiselectable, Orientation, ariaOrientation, ariaPlaceholder, ariaPressed, ariaReadonly, ariaRequired, ariaSelected, Sort, ariaSort, ariaValuemax, ariaValuemin, ariaValuenow, ariaValuetext

## Live Region Attributes
The purpose of these attributes is to indicate that content changes may occur without the element having focus, and to provide assistive technologies with information on how to process those content updates.

@docs ariaAtomic, ariaBusy, ariaLive, ariaRelevant

## Drag-and-Drop Attributes
Attributes which indicate information about drag-and-drop interface elements, such as draggable elements and their drop targets.
@docs DropEffect, ariaDropeffect, ariaGrabbed

## Relationship Attributes
@docs ariaActivedescendant, ariaColcount, ariaColindex, ariaColspan, ariaControls, ariaDescribedby, ariaDetails, ariaErrormessage, ariaFlowto, ariaLabelledby, ariaOwns, ariaPosinset, ariaRowcount, ariaRowindex, ariaRowspan, ariaSetsize

## Helpers
@docs boolToTristate

Copyright © 2013-2017 W3C® (MIT, ERCIM, Keio, Beihang). This software or document includes material copied from or derived from [Accessible Rich Internet Applications (WAI-ARIA) 1.1][WAI-ARIA].

[WAI-ARIA]: https://w3c.github.io/aria/aria/aria.html
-}

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


{-| A unique identifier for a particular DOM node. When you create
`<div id="my-thing"></div>` you would refer to it with the `Id` `"my-thing"`.
-}
type alias Id =
    String


{-| Value representing true or false, with an intermediate “mixed” value.
-}
type Tristate
    = On
    | Off
    | Mixed


{-| Convert boolean value to On or Off.
-}
boolToTristate : Bool -> Tristate
boolToTristate bool =
    if bool then
        On
    else
        Off


boolAttribute : String -> Bool -> Attribute msg
boolAttribute name bool =
    attribute name
        (if bool then
            "true"
         else
            "false"
        )


maybeBoolAttribute : String -> Maybe Bool -> Attribute msg
maybeBoolAttribute name maybeBool =
    let
        val =
            case maybeBool of
                Just True ->
                    "true"

                Just False ->
                    "false"

                Nothing ->
                    "undefined"
    in
        attribute name val


maybeTristateAttribute : String -> Maybe Tristate -> Attribute msg
maybeTristateAttribute name maybeTristate =
    let
        val =
            case maybeTristate of
                Just On ->
                    "true"

                Just Off ->
                    "false"

                Just Mixed ->
                    "mixed"

                Nothing ->
                    "undefined"
    in
        attribute name val


idListAttribute : String -> List Id -> Attribute msg
idListAttribute name ids =
    attribute name (String.join " " ids)


intAttribute : String -> Int -> Attribute msg
intAttribute name int =
    attribute name (toString int)



-- GLOBAL STATES AND PROPERTIES


{-|
* `CurrentPage` – Represents the current page within a set of pages.
* `CurrentStep` – Represents the current step within a process.
* `CurrentLocation` – Represents the current location within an environment or context.
* `CurrentDate` – Represents the current date within a collection of dates.
* `CurrentTime` – Represents the current time within a set of times.
* `CurrentOther` – Represents the current item within a set.
-}
type CurrentType
    = CurrentPage
    | CurrentStep
    | CurrentLocation
    | CurrentDate
    | CurrentTime
    | CurrentOther


{-| Indicates the element that represents the current item within a container or set of related elements.

Represents [`aria-current`](https://w3c.github.io/aria/aria/aria.html#aria-current) attribute.
-}
ariaCurrent : Maybe CurrentType -> Attribute msg
ariaCurrent current =
    let
        val =
            case current of
                Just CurrentPage ->
                    "page"

                Just CurrentStep ->
                    "step"

                Just CurrentLocation ->
                    "location"

                Just CurrentDate ->
                    "date"

                Just CurrentTime ->
                    "time"

                Just CurrentOther ->
                    "true"

                Nothing ->
                    "false"
    in
        attribute "aria-current" val


{-| Indicates keyboard shortcuts that an author has implemented to activate or give focus to an element.

The value of the `aria-keyshortcuts` attribute is a space-delimited list of keyboard shortcuts that can be pressed to activate a command or textbox widget. The keys defined in the shortcuts represent the physical keys pressed and not the actual characters generated. Each keyboard shortcut consists of one or more tokens delimited by the plus sign (“+”) representing zero or more modifier keys and exactly one non-modifier key that must be pressed simultaneously to activate the given shortcut.

Authors MUST specify modifier keys exactly according to the [UI Events KeyboardEvent key Values][uievents-key] spec - for example, “Alt”, “Control”, “Shift”, “Meta”, or “AltGraph”. Note that Meta corresponds to the Command key, and Alt to the Option key, on Apple computers.

Represents [`aria-keyshortcuts`](https://w3c.github.io/aria/aria/aria.html#aria-keyshortcuts) attribute.

[uievents-key]: https://www.w3.org/TR/uievents-key/
-}
ariaKeyshortcuts : String -> Attribute msg
ariaKeyshortcuts keyshortcuts =
    attribute "aria-keyshortcuts" keyshortcuts


{-| Defines a human-readable, author-localized description for the role of an element.

Represents [`aria-roledescription`](https://w3c.github.io/aria/aria/aria.html#aria-roledescription) attribute.
-}
ariaRoledescription : String -> Attribute msg
ariaRoledescription roledescription =
    attribute "aria-roledescription" roledescription



-- WIDGET ATTRIBUTES


{-|
* `AutoCompleteInline` – When a user is providing input, text suggesting one way to complete the provided input may be dynamically inserted after the caret.
* `AutoCompleteList` – When a user is providing input, an element containing a collection of values that could complete the provided input may be displayed.
* `AutoCompleteBoth` – When a user is providing input, an element containing a collection of values that could complete the provided input may be displayed. If displayed, one value in the collection is automatically selected, and the text needed to complete the automatically selected value appears after the caret in the input.
-}
type AutocompleteType
    = AutocompleteInline
    | AutocompleteList
    | AutocompleteBoth


{-| Indicates whether inputting text could trigger display of one or more predictions of the user’s intended value for an input and specifies how predictions would be presented if they are made.

Represents [`aria-autocomplete`](https://w3c.github.io/aria/aria/aria.html#aria-autocomplete) attribute.
-}
ariaAutocomplete : Maybe AutocompleteType -> Attribute msg
ariaAutocomplete autocomplete =
    let
        val =
            case autocomplete of
                Just AutocompleteInline ->
                    "inline"

                Just AutocompleteList ->
                    "list"

                Just AutocompleteBoth ->
                    "both"

                Nothing ->
                    "none"
    in
        attribute "autocomplete" val


{-| Indicates the current “checked” state of checkboxes, radio buttons, and other widgets. See related `aria-pressed` and `aria-selected`.

Represents [`aria-checked`](https://w3c.github.io/aria/aria/aria.html#aria-checked) attribute.
-}
ariaChecked : Maybe Tristate -> Attribute msg
ariaChecked checked =
    maybeTristateAttribute "aria-checked" checked


{-| Indicates that the element is perceivable but disabled, so it is not editable or otherwise operable. See related `aria-hidden` and `aria-readonly`.

Represents [`aria-disabled`](https://w3c.github.io/aria/aria/aria.html#aria-disabled) attribute.
-}
ariaDisabled : Bool -> Attribute msg
ariaDisabled disabled =
    boolAttribute "aria-disabled" disabled


{-| Indicates whether the element, or another grouping element it controls, is currently expanded or collapsed.

Represents [`aria-expanded`](https://w3c.github.io/aria/aria/aria.html#aria-expanded) attribute.
-}
ariaExpanded : Maybe Bool -> Attribute msg
ariaExpanded expanded =
    maybeBoolAttribute "aria-expanded" expanded


{-|
* `PopupMenu` – Indicates the popup is a `menu`.
* `PopupListbox` – Indicates the popup is a listbox.
* `PopupTree` – Indicates the popup is a tree.
* `PopupGrid` – Indicates the popup is a grid.
* `PopupDialog` – Indicates the popup is a dialog.
-}
type PopupType
    = PopupMenu
    | PopupListbox
    | PopupTree
    | PopupGrid
    | PopupDialog


{-| Indicates the availability and type of interactive popup element, such as menu or dialog, that can be triggered by an element.

Represents [`aria-haspopup`](https://w3c.github.io/aria/aria/aria.html#aria-haspopup) attribute.
-}
ariaHaspopup : Maybe PopupType -> Attribute msg
ariaHaspopup popup =
    let
        val =
            case popup of
                Nothing ->
                    "false"

                Just PopupMenu ->
                    "menu"

                Just PopupListbox ->
                    "listbox"

                Just PopupTree ->
                    "tree"

                Just PopupGrid ->
                    "grid"

                Just PopupDialog ->
                    "dialog"
    in
        attribute "aria-haspopup" val


{-| Indicates whether the element is exposed to an accessibility API. See related `aria-disabled`.

Represents [`aria-hidden`](https://w3c.github.io/aria/aria/aria.html#aria-hidden) attribute.
-}
ariaHidden : Maybe Bool -> Attribute msg
ariaHidden hidden =
    maybeBoolAttribute "aria-hidden" hidden


{-|
* `InvalidGrammar` – A grammatical error was detected.
* `InvalidSpelling` – A spelling error was detected.
* `InvalidOther` – The value entered by the user has failed validation.
-}
type InvalidityReason
    = InvalidGrammar
    | InvalidSpelling
    | InvalidOther


{-| Indicates the entered value does not conform to the format expected by the application. See related `aria-errormessage`.

Represents [`aria-invalid`](https://w3c.github.io/aria/aria/aria.html#aria-invalid) attribute.
-}
ariaInvalid : Maybe InvalidityReason -> Attribute msg
ariaInvalid invalidity =
    let
        val =
            case invalidity of
                Just InvalidGrammar ->
                    "grammar"

                Just InvalidSpelling ->
                    "spelling"

                Just InvalidOther ->
                    "true"

                Nothing ->
                    "false"
    in
        attribute "aria-invalid" val


{-| Defines a string value that labels the current element. See related `aria-labelledby`.

Represents [`aria-label`](https://w3c.github.io/aria/aria/aria.html#aria-label) attribute.
-}
ariaLabel : String -> Attribute msg
ariaLabel label =
    attribute "aria-label" label


{-| Defines the hierarchical level of an element within a structure.

This can be applied inside trees to tree items, to headings inside a document, to nested grids, nested tablists and to other structural items that may appear inside a container or participate in an ownership hierarchy. The value for `aria-level` is an integer greater than or equal to 1.

Levels increase with depth. If the DOM ancestry does not accurately represent the level, authors SHOULD explicitly define the `aria-level` attribute.

Represents [`aria-level`](https://w3c.github.io/aria/aria/aria.html#aria-level) attribute.
-}
ariaLevel : Int -> Attribute msg
ariaLevel level =
    intAttribute "aria-level" level


{-| Indicates whether an element is modal when displayed.

Represents [`aria-modal`](https://w3c.github.io/aria/aria/aria.html#aria-modal) attribute.
-}
ariaModal : Bool -> Attribute msg
ariaModal modal =
    boolAttribute "aria-modal" modal


{-| Indicates whether a text box accepts multiple lines of input or only a single line.

Represents [`aria-multiline`](https://w3c.github.io/aria/aria/aria.html#aria-multiline) attribute.
-}
ariaMultiline : Bool -> Attribute msg
ariaMultiline multiline =
    boolAttribute "aria-multiline" multiline


{-| Indicates that the user may select more than one item from the current selectable descendants.

Represents [`aria-multiselectable`](https://w3c.github.io/aria/aria/aria.html#aria-multiselectable) attribute.
-}
ariaMultiselectable : Bool -> Attribute msg
ariaMultiselectable multiselectable =
    boolAttribute "aria-multiselectable" multiselectable


{-|
* `Horizontal` – The element is oriented horizontally.
* `Vertical` – The element is oriented vertically.
-}
type Orientation
    = Horizontal
    | Vertical


{-| Indicates whether the element and orientation is horizontal, vertical, or undefined.

Represents [`aria-orientation`](https://w3c.github.io/aria/aria/aria.html#aria-orientation) attribute.
-}
ariaOrientation : Maybe Orientation -> Attribute msg
ariaOrientation orientation =
    let
        val =
            case orientation of
                Just Horizontal ->
                    "horizontal"

                Just Vertical ->
                    "vertical"

                Nothing ->
                    "undefined"
    in
        attribute "aria-orientation" val


{-| Defines a short hint (a word or short phrase) intended to aid the user with data entry when the control has no value. A hint could be a sample value or a brief description of the expected format.

Represents [`aria-placeholder`](https://w3c.github.io/aria/aria/aria.html#aria-placeholder) attribute.
-}
ariaPlaceholder : String -> Attribute msg
ariaPlaceholder placeholder =
    attribute "aria-placeholder" placeholder


{-| Indicates the current “pressed” state of toggle buttons. See related `aria-checked` and `aria-selected`.

* `Just Off` – The element supports being pressed but is not currently pressed.
* `Just Mixed` – Indicates a mixed mode value for a tri-state toggle button.
* `Just On` – The element is pressed.
* `Nothing` (default) – The element does not support being pressed.

Represents [`aria-pressed`](https://w3c.github.io/aria/aria/aria.html#aria-pressed) attribute.
-}
ariaPressed : Maybe Tristate -> Attribute msg
ariaPressed pressed =
    maybeTristateAttribute "aria-pressed" pressed


{-| Indicates that the element is not editable, but is otherwise operable. See related `aria-disabled`.

Represents [`aria-readonly`](https://w3c.github.io/aria/aria/aria.html#aria-readonly) attribute.
-}
ariaReadonly : Bool -> Attribute msg
ariaReadonly readonly =
    boolAttribute "aria-readonly" readonly


{-| Indicates that user input is required on the element before a form may be submitted.

Represents [`aria-required`](https://w3c.github.io/aria/aria/aria.html#aria-required) attribute.
-}
ariaRequired : Bool -> Attribute msg
ariaRequired required =
    boolAttribute "aria-required" required


{-| Indicates the current “selected” state of various widgets. See related `aria-checked` and `aria-pressed`.

* `Just False` – The selectable element is not selected.
* `Just True` – The selectable element is selected.
* `Nothing` (default) – The element is not selectable.

Represents [`aria-selected`](https://w3c.github.io/aria/aria/aria.html#aria-selected) attribute.
-}
ariaSelected : Maybe Bool -> Attribute msg
ariaSelected selected =
    maybeBoolAttribute "aria-selected" selected


{-|
* `SortAscending` – Items are sorted in ascending order by this column.
* `SortDescending` – Items are sorted in descending order by this column.
* `SortOther` – A sort algorithm other than ascending or descending has been applied.
-}
type Sort
    = SortAscending
    | SortDescending
    | SortOther


{-| Indicates if items in a table or grid are sorted in ascending or descending order.

Represents [`aria-sort`](https://w3c.github.io/aria/aria/aria.html#aria-sort) attribute.
-}
ariaSort : Maybe Sort -> Attribute msg
ariaSort sort =
    let
        val =
            case sort of
                Just SortAscending ->
                    "ascending"

                Just SortDescending ->
                    "descending"

                Just SortOther ->
                    "other"

                Nothing ->
                    "none"
    in
        attribute "aria-sort" val


{-| Defines the maximum allowed value for a range widget.

Represents [`aria-valuemax`](https://w3c.github.io/aria/aria/aria.html#aria-valuemax) attribute.
-}
ariaValuemax : Float -> Attribute msg
ariaValuemax max =
    attribute "aria-valuemax" (toString max)


{-| Defines the minimum allowed value for a range widget.

Represents [`aria-valuemin`](https://w3c.github.io/aria/aria/aria.html#aria-valuemin) attribute.
-}
ariaValuemin : Float -> Attribute msg
ariaValuemin min =
    attribute "aria-valuemin" (toString min)


{-| Defines the current value for a range widget. See related `aria-valuetext`.

Represents [`aria-valuenow`](https://w3c.github.io/aria/aria/aria.html#aria-valuenow) attribute.
-}
ariaValuenow : Float -> Attribute msg
ariaValuenow now =
    attribute "aria-valuenow" (toString now)


{-| Defines the human readable text alternative of `aria-valuenow` for a range widget.

Represents [`aria-valuetext`](https://w3c.github.io/aria/aria/aria.html#aria-valuetext) attribute.
-}
ariaValuetext : String -> Attribute msg
ariaValuetext text =
    attribute "aria-valuetext" text



-- LIVE REGION ATTRIBUTES


{-| Indicates whether assistive technologies will present all, or only parts of, the changed region based on the change notifications defined by the `aria-relevant` attribute.

Represents [`aria-atomic`](https://w3c.github.io/aria/aria/aria.html#aria-atomic) attribute.
-}
ariaAtomic : Bool -> Attribute msg
ariaAtomic atomic =
    boolAttribute "aria-atomic" atomic


{-| Indicates an element is being modified and that assistive technologies MAY want to wait until the modifications are complete before exposing them to the user.

Represents [`aria-busy`](https://w3c.github.io/aria/aria/aria.html#aria-busy) attribute.
-}
ariaBusy : Bool -> Attribute msg
ariaBusy busy =
    boolAttribute "aria-busy" busy


{-|
* `LiveAssertive` – Indicates that updates to the region have the highest priority and should be presented the user immediately.
* `LiveOff` (default) – Indicates that updates to the region should not be presented to the user unless the used is currently focused on that region.
* `LivePolite` – Indicates that updates to the region should be presented at the next graceful opportunity, such as at the end of speaking the current sentence or when the user pauses typing.
-}
type Live
    = LiveAssertive
    | LiveOff
    | LivePolite


{-| Indicates that an element will be updated, and describes the types of updates the user agents, assistive technologies, and user can expect from the live region.

Represents [`aria-live`](https://w3c.github.io/aria/aria/aria.html#aria-live) attribute.
-}
ariaLive : Live -> Attribute msg
ariaLive live =
    let
        val =
            case live of
                LiveAssertive ->
                    "assertive"

                LiveOff ->
                    "off"

                LivePolite ->
                    "polite"
    in
        attribute "aria-live" val


{-|
* `RelevantAdditions` – Element nodes are added to the accessibility tree within the live region.
* `RelevantRemovals` – Text content, a text alternative, or an element node within the live region is removed from the accessibility tree.
* `RelevantText` – Text content or a text alternative is added to any descendant in the accessibility tree of the live region.
-}
type Relevant
    = RelevantAdditions
    | RelevantRemovals
    | RelevantText


{-| Indicates what notifications the user agent will trigger when the accessibility tree within a live region is modified. See related `aria-atomic`.

Represents [`aria-relevant`](https://w3c.github.io/aria/aria/aria.html#aria-relevant) attribute.
-}
ariaRelevant : List Relevant -> Attribute msg
ariaRelevant relevant =
    let
        toString rel =
            case rel of
                RelevantAdditions ->
                    "additions"

                RelevantRemovals ->
                    "removals"

                RelevantText ->
                    "text"
    in
        attribute "aria-relevant" (String.join " " (List.map toString relevant))



-- DRAG-AND-DROP ATTRIBUTES


{-|
* `DropCopy` – A duplicate of the source object will be dropped into the target.
* `DropExecute` – A function supported by the drop target is executed, using the drag source as an input.
* `DropLink` – A reference or shortcut to the dragged object will be created in the target object.
* `DropMove` – The source object will be removed from its current location and dropped into the target.
* `DropPopup` – There is a popup menu or dialog that allows the user to choose one of the drag operations (`DropCopy`, `DropMove`, `DropLink`, `DropExecute`) and any other drag functionality, such as cancel.
-}
type DropEffect
    = DropCopy
    | DropExecute
    | DropLink
    | DropMove
    | DropPopup


{-| [*Deprecated in ARIA 1.1*] Indicates what functions can be performed when a dragged object is released on the drop target.

Represents [`aria-dropeffect`](https://w3c.github.io/aria/aria/aria.html#aria-dropeffect) attribute.
-}
ariaDropeffect : List DropEffect -> Attribute msg
ariaDropeffect effects =
    let
        toString effect =
            case effect of
                DropCopy ->
                    "copy"

                DropExecute ->
                    "execute"

                DropLink ->
                    "link"

                DropMove ->
                    "move"

                DropPopup ->
                    "popup"

        val =
            if List.isEmpty effects then
                "none"
            else
                String.join " " (List.map toString effects)
    in
        attribute "aria-dropeffect" val


{-| [*Deprecated in ARIA 1.1*] Indicates an element’s “grabbed” state in a drag-and-drop operation.

Represents [`aria-grabbed`](https://w3c.github.io/aria/aria/aria.html#aria-grabbed) attribute.
-}
ariaGrabbed : Maybe Bool -> Attribute msg
ariaGrabbed grabbed =
    maybeBoolAttribute "aria-grabbed" grabbed



-- RELATIONSHIP ATTRIBUTES


{-| Identifies the currently active element when DOM focus is on a `composite` widget, `textbox`, `group`, or `application`.

Represents [`aria-activedescendant`](https://w3c.github.io/aria/aria/aria.html#aria-activedescendant) attribute.
-}
ariaActivedescendant : Id -> Attribute msg
ariaActivedescendant id =
    attribute "aria-activedescendant" id


{-| Defines the total number of columns in a table, `grid`, or `treegrid`. See related `aria-colindex`.

Represents [`aria-colcount`](https://w3c.github.io/aria/aria/aria.html#aria-colcount) attribute.
-}
ariaColcount : Int -> Attribute msg
ariaColcount count =
    intAttribute "aria-colcount" count


{-| Defines an element’s column index or position with respect to the total number of columns within a table, `grid`, or `treegrid`. See related `aria-colcount` and `aria-colspan`.

Represents [`aria-colindex`](https://w3c.github.io/aria/aria/aria.html#aria-colindex) attribute.
-}
ariaColindex : Int -> Attribute msg
ariaColindex index =
    intAttribute "aria-colindex" index


{-| Defines the number of columns spanned by a cell or gridcell within a table, `grid`, or `treegrid`. See related `aria-colindex` and `aria-rowspan`.

Represents [`aria-colspan`](https://w3c.github.io/aria/aria/aria.html#aria-colspan) attribute.
-}
ariaColspan : Int -> Attribute msg
ariaColspan span =
    intAttribute "aria-colspan" span


{-| Identifies the element (or elements) whose contents or presence are controlled by the current element. See related `aria-owns`.

Represents [`aria-controls`](https://w3c.github.io/aria/aria/aria.html#aria-controls) attribute.
-}
ariaControls : List Id -> Attribute msg
ariaControls ids =
    idListAttribute "aria-controls" ids


{-| Identifies the element (or elements) that describes the object. See related `aria-labelledby`.

Represents [`aria-describedby`](https://w3c.github.io/aria/aria/aria.html#aria-describedby) attribute.
-}
ariaDescribedby : List Id -> Attribute msg
ariaDescribedby ids =
    idListAttribute "aria-describedby" ids


{-| Identifies the element that provides a detailed, extended description for the object. See related `aria-describedby`.

Represents [`aria-details`](https://w3c.github.io/aria/aria/aria.html#aria-details) attribute.
-}
ariaDetails : Id -> Attribute msg
ariaDetails id =
    attribute "aria-details" id


{-| Identifies the element that provides an error message for the object. See related `aria-invalid` and `aria-describedby`.

Represents [`aria-errormessage`](https://w3c.github.io/aria/aria/aria.html#aria-errormessage) attribute.
-}
ariaErrormessage : Id -> Attribute msg
ariaErrormessage id =
    attribute "aria-errormessage" id


{-| Identifies the next element (or elements) in an alternate reading order of content which, at the user’s discretion, allows assistive technology to override the general default of reading in document source order.

Represents [`aria-flowto`](https://w3c.github.io/aria/aria/aria.html#aria-flowto) attribute.
-}
ariaFlowto : List Id -> Attribute msg
ariaFlowto ids =
    idListAttribute "aria-flowto" ids


{-| Identifies the element (or elements) that labels the current element. See related `aria-describedby`.

Represents [`aria-labelledby`](https://w3c.github.io/aria/aria/aria.html#aria-labelledby) attribute.
-}
ariaLabelledby : List Id -> Attribute msg
ariaLabelledby ids =
    idListAttribute "aria-labelledby" ids


{-| Identifies an element (or elements) in order to define a visual, functional, or contextual parent/child relationship between DOM elements where the DOM hierarchy cannot be used to represent the relationship. See related `aria-controls`.

Represents [`aria-owns`](https://w3c.github.io/aria/aria/aria.html#aria-owns) attribute.
-}
ariaOwns : List Id -> Attribute msg
ariaOwns ids =
    idListAttribute "aria-owns" ids


{-| Defines an element’s number or position in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM. See related `aria-setsize`.

Represents [`aria-posinset`](https://w3c.github.io/aria/aria/aria.html#aria-posinset) attribute.
-}
ariaPosinset : Int -> Attribute msg
ariaPosinset pos =
    intAttribute "aria-posinset" pos


{-| Defines the total number of rows in a table, `grid`, or `treegrid`. See related `aria-rowindex`.

Represents [`aria-rowcount`](https://w3c.github.io/aria/aria/aria.html#aria-rowcount) attribute.
-}
ariaRowcount : Int -> Attribute msg
ariaRowcount count =
    intAttribute "aria-rowcount" count


{-| Defines an element’s row index or position with respect to the total number of rows within a table, `grid`, or `treegrid`. See related `aria-rowcount` and `aria-rowspan`.

Represents [`aria-rowindex`](https://w3c.github.io/aria/aria/aria.html#aria-rowindex) attribute.
-}
ariaRowindex : Int -> Attribute msg
ariaRowindex index =
    intAttribute "aria-rowindex" index


{-| Defines the number of rows spanned by a cell or gridcell within a table, `grid`, or `treegrid`. See related `aria-rowindex` and `aria-colspan`.

Represents [`aria-rowspan`](https://w3c.github.io/aria/aria/aria.html#aria-rowspan) attribute.
-}
ariaRowspan : Int -> Attribute msg
ariaRowspan span =
    intAttribute "aria-rowspan" span


{-| Defines the number of items in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM. See related `aria-posinset`.

Represents [`aria-setsize`](https://w3c.github.io/aria/aria/aria.html#aria-setsize) attribute.
-}
ariaSetsize : Int -> Attribute msg
ariaSetsize size =
    intAttribute "aria-setsize" size

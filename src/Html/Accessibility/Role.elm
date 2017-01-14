module Html.Accessibility.Role exposing (role, Role(..))

{-| This module provides non-abstract [WAI-ARIA] roles, as well as an attribute for assigning them to elements.

@docs Role, role

Copyright © 2013-2017 W3C® (MIT, ERCIM, Keio, Beihang). This software or document includes material copied from or derived from [Accessible Rich Internet Applications (WAI-ARIA) 1.1][WAI-ARIA].

[WAI-ARIA]: https://w3c.github.io/aria/aria/aria.html
-}

import Basics exposing (toString)
import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import String exposing (toLower)


{-| Non-abstract roles. For information about them consult the [specification](https://w3c.github.io/aria/aria/aria.html#roles_categorization).
-}
type Role
    = -- # Widget Roles
      -- The following roles act as standalone user interface widgets or as part of larger, composite widgets.
      Alert
    | AlertDialog
    | Button
    | CheckBox
    | Dialog
    | GridCell
    | Link
    | Log
    | Marquee
    | MenuItem
    | MenuItemCheckBox
    | MenuItemRadio
    | Option
    | ProgressBar
    | Radio
    | ScrollBar
    | SearchBox
    | Slider
    | SpinButton
    | Status
    | Switch
    | Tab
    | TabPanel
    | TextBox
    | Timer
    | Tooltip
    | TreeItem
      -- The following roles act as composite user interface widgets. These roles typically act as containers that manage other, contained widgets.
    | ComboBox
    | Grid
    | ListBox
    | Menu
    | MenuBar
    | RadioGroup
    | Tablist
    | Tree
    | TreeGrid
      -- # Document Structure
      -- The following roles describe structures that organize content in a page. Document structures are not usually interactive.
    | Application
    | Article
    | Cell
    | ColumnHeader
    | Definition
    | Directory
    | Document
    | Feed
    | Figure
    | Group
    | Heading
    | Img
    | List
    | ListItem
    | Math
    | None
    | Note
    | Presentation
    | Region
    | Row
    | RowGroup
    | RowHeader
    | Separator
    | Table
    | Term
    | ToolBar
      -- # Landmark Roles
      -- The following roles are regions of the page intended as navigational landmarks.
    | Banner
    | Complementary
    | ContentInfo
    | Form
    | Main
    | Navigation
    | Search


{-| Attribute for setting a role of an element.
-}
role : Role -> Attribute msg
role r =
    attribute "role" (toLower (toString r))

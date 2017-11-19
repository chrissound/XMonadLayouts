# XMonad Middle Column Layout (4k / Large monitor friendly)

## Installation

    cd ~/.xmonad
    mkdir -p lib
    cd lib
    git clone https://github.com/chrissound/XMonadLayouts .

This above will add the modules to be acccessible to your xmonad.hs

You then need to add the follow imports:

    import           MiddleColumn
    import WindowColumn
    import WindowColumn as Column (Column(..))
    import XMonad.Actions.Submap

This helper function:

    defaultThreeColumn :: (Float, Float, Float)
    defaultThreeColumn = (0.15, 0.65, 0.2)

Add the layout to your config (by adding it within `layoutHook`), for example:

    layoutHook = desktopLayoutModifiers $ getMiddleColumnSaneDefault 2 0.15 defaultThreeColumn)

Now you just need to set the keybindings. I'm using a dvorak layout keyboard so these perhaps could be set to something else if using qwerty:

    -- Resize left / right column width individually
    , ((mod1Mask, xK_i), sendMessage IncrementLeftColumnContainerWidth)
    , ((mod1Mask, xK_u), sendMessage DecrementLeftColumnContainerWidth)
    , ((mod1Mask, xK_d), sendMessage IncrementRightColumnContainerWidth)
    , ((mod1Mask, xK_h), sendMessage DecrementRightColumnContainerWidth)
    , ((mod1Mask, xK_y), sendMessage ResetColumnContainerWidth)

    -- Modify column pin count
    , ((modm , xK_period), sendMessage IncrementLeftColumnContainer)
    , ((modm , xK_y), sendMessage IncrementRightColumnContainer)
    , ((modm  .|. shiftMask, xK_y), sendMessage ResetColumnContainer)

    -- Swop column positions
    , ((modm .|. shiftMask, xK_i), sendMessage SwopLeftColumn)
    , ((modm .|. shiftMask, xK_d), sendMessage SwopRightColumn)
    , ((mod1Mask, xK_r), sendMessage ResetColumn)

    -- Modify main column width
    , ((modm, xK_i), sendMessage Shrink)
    , ((modm, xK_d), sendMessage Expand)

    -- Focus nth window
    , ((modm .|. controlMask, xK_h), sendMessage $ FocusLeft (1 :: Int))
    , ((modm .|. controlMask, xK_t), sendMessage $ FocusLeft (2 :: Int))
    , ((modm .|. controlMask, xK_n), sendMessage $ FocusLeft (3 :: Int))
    , ((modm .|. controlMask, xK_s), sendMessage $ FocusLeft (4 :: Int))
    , ((modm .|. controlMask, xK_g), sendMessage $ FocusRight (1 :: Int))
    , ((modm .|. controlMask, xK_c), sendMessage $ FocusRight (2 :: Int))
    , ((modm .|. controlMask, xK_r), sendMessage $ FocusRight (3 :: Int))
    , ((modm .|. controlMask, xK_l), sendMessage $ FocusRight (4 :: Int))

    -- Swap master window with nth side window
    , ((mod1Mask .|. shiftMask, xK_h), sendMessage $ SwopLeft (1 :: Int))
    , ((mod1Mask .|. shiftMask, xK_t), sendMessage $ SwopLeft (2 :: Int))
    , ((mod1Mask .|. shiftMask, xK_n), sendMessage $ SwopLeft (3 :: Int))
    , ((mod1Mask .|. shiftMask, xK_s), sendMessage $ SwopLeft (4 :: Int))
    , ((mod1Mask .|. shiftMask, xK_g), sendMessage $ SwopRight (1 :: Int))
    , ((mod1Mask .|. shiftMask, xK_c), sendMessage $ SwopRight (2 :: Int))
    , ((mod1Mask .|. shiftMask, xK_r), sendMessage $ SwopRight (3 :: Int))
    , ((mod1Mask .|. shiftMask, xK_l), sendMessage $ SwopRight (4 :: Int))


## Functionality:
- Main rectangle that is centered.
- Additional rows can be added in the middle column.
- Set a specific ratio between rows in the middle column can be set when there are two or three windows in the middle column.
- Pin the left or right to have a maximum amount windows. (I usuall have two left pinned windows). So for example you can pin the left column to only have a maximum of two windows, of which additional windows would accumulate on the right column.
- Swop the left or right column with the middle column.
- Swop or focus a window in the left or right column by the position in the column. For example you can focus the 3rd Window in the left column.
- Set the left or right column width individually. 

## Demo
[![Video demo](http://img.youtube.com/vi/5gScoAp2BBQ/0.jpg)](http://www.youtube.com/watch?v=5gScoAp2BBQ "Video demo") 

## Example Screenshots
![MiddleColumn Example](http://i.imgur.com/OrmshtY.jpg)
![MiddleColumn Example](http://i.imgur.com/m5EtcT1.jpg)
![MiddleColumn Example](http://i.imgur.com/uFD87WR.jpg)
![MiddleColumn Example 2](http://i.imgur.com/FyHpotk.jpg)

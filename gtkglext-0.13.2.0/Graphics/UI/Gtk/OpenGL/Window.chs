{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) OpenGL Extension: GLWindow
--
--  Author : Duncan Coutts
--
--  Created: 9 June 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- OpenGL window which is located on-screen
--
module Graphics.UI.Gtk.OpenGL.Window (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Drawable'
-- |         +----GLWindow
-- @

-- * Types
  GLWindow,
  GLWindowClass,
  castToGLWindow,

-- * Constructors
  glWindowNew,

-- * Methods
  glWindowGetWindow,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject, wrapNewGObject)
{#import Graphics.UI.Gtk.OpenGL.Types#}

{# context lib="gtkglext" prefix="gdk" #}

instance GLDrawableClass GLWindow

--------------------
-- Constructors

-- | Creates an on-screen rendering area.
--
glWindowNew :: GLConfig
 -> DrawWindow -- ^ @window@ - the 'DrawWindow' to be used as the rendering
                 -- area.
 -> IO GLWindow
glWindowNew glconfig window =
  wrapNewGObject mkGLWindow $
  {# call gdk_gl_window_new #}
    (toGLConfig glconfig)
    window
    nullPtr

--------------------
-- Methods

-- | Returns the 'DrawWindow' associated with a 'GLWindow'.
--
glWindowGetWindow :: GLWindow -> IO DrawWindow
glWindowGetWindow self =
  makeNewGObject mkDrawWindow $
  {# call gdk_gl_window_get_window #}
    (toGLWindow self)

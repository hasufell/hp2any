{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module Graphics.UI.Gtk.OpenGL.Types (

  module Graphics.UI.GtkInternals,
  GLPixmap(GLPixmap), GLPixmapClass,
  toGLPixmap, 
  mkGLPixmap, unGLPixmap,
  castToGLPixmap, gTypeGLPixmap,
  GLWindow(GLWindow), GLWindowClass,
  toGLWindow, 
  mkGLWindow, unGLWindow,
  castToGLWindow, gTypeGLWindow,
  GLContext(GLContext), GLContextClass,
  toGLContext, 
  mkGLContext, unGLContext,
  castToGLContext, gTypeGLContext,
  GLConfig(GLConfig), GLConfigClass,
  toGLConfig, 
  mkGLConfig, unGLConfig,
  castToGLConfig, gTypeGLConfig,
  GLDrawable(GLDrawable), GLDrawableClass,
  toGLDrawable, 
  mkGLDrawable, unGLDrawable,
  castToGLDrawable, gTypeGLDrawable
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
-- TODO work around cpphs https://ghc.haskell.org/trac/ghc/ticket/13553
#if __GLASGOW_HASKELL__ >= 707 || __GLASGOW_HASKELL__ == 0
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif

import Foreign.C.Types    (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType  (GType, typeInstanceIsA)
{#import Graphics.UI.GtkInternals#}

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- ******************************************************************* GLPixmap

{#pointer *GdkGLPixmap as GLPixmap foreign newtype #} deriving (Eq,Ord)

mkGLPixmap = (GLPixmap, objectUnrefFromMainloop)
unGLPixmap (GLPixmap o) = o

class DrawableClass o => GLPixmapClass o
toGLPixmap :: GLPixmapClass o => o -> GLPixmap
toGLPixmap = unsafeCastGObject . toGObject

instance GLPixmapClass GLPixmap
instance DrawableClass GLPixmap
instance GObjectClass GLPixmap where
  toGObject = GObject . castForeignPtr . unGLPixmap
  unsafeCastGObject = GLPixmap . castForeignPtr . unGObject

castToGLPixmap :: GObjectClass obj => obj -> GLPixmap
castToGLPixmap = castTo gTypeGLPixmap "GLPixmap"

gTypeGLPixmap :: GType
gTypeGLPixmap =
  {# call fun unsafe gdk_gl_pixmap_get_type #}

-- ******************************************************************* GLWindow

{#pointer *GdkGLWindow as GLWindow foreign newtype #} deriving (Eq,Ord)

mkGLWindow = (GLWindow, objectUnrefFromMainloop)
unGLWindow (GLWindow o) = o

class DrawableClass o => GLWindowClass o
toGLWindow :: GLWindowClass o => o -> GLWindow
toGLWindow = unsafeCastGObject . toGObject

instance GLWindowClass GLWindow
instance DrawableClass GLWindow
instance GObjectClass GLWindow where
  toGObject = GObject . castForeignPtr . unGLWindow
  unsafeCastGObject = GLWindow . castForeignPtr . unGObject

castToGLWindow :: GObjectClass obj => obj -> GLWindow
castToGLWindow = castTo gTypeGLWindow "GLWindow"

gTypeGLWindow :: GType
gTypeGLWindow =
  {# call fun unsafe gdk_gl_window_get_type #}

-- ****************************************************************** GLContext

{#pointer *GdkGLContext as GLContext foreign newtype #} deriving (Eq,Ord)

mkGLContext = (GLContext, objectUnrefFromMainloop)
unGLContext (GLContext o) = o

class GObjectClass o => GLContextClass o
toGLContext :: GLContextClass o => o -> GLContext
toGLContext = unsafeCastGObject . toGObject

instance GLContextClass GLContext
instance GObjectClass GLContext where
  toGObject = GObject . castForeignPtr . unGLContext
  unsafeCastGObject = GLContext . castForeignPtr . unGObject

castToGLContext :: GObjectClass obj => obj -> GLContext
castToGLContext = castTo gTypeGLContext "GLContext"

gTypeGLContext :: GType
gTypeGLContext =
  {# call fun unsafe gdk_gl_context_get_type #}

-- ******************************************************************* GLConfig

{#pointer *GdkGLConfig as GLConfig foreign newtype #} deriving (Eq,Ord)

mkGLConfig = (GLConfig, objectUnrefFromMainloop)
unGLConfig (GLConfig o) = o

class GObjectClass o => GLConfigClass o
toGLConfig :: GLConfigClass o => o -> GLConfig
toGLConfig = unsafeCastGObject . toGObject

instance GLConfigClass GLConfig
instance GObjectClass GLConfig where
  toGObject = GObject . castForeignPtr . unGLConfig
  unsafeCastGObject = GLConfig . castForeignPtr . unGObject

castToGLConfig :: GObjectClass obj => obj -> GLConfig
castToGLConfig = castTo gTypeGLConfig "GLConfig"

gTypeGLConfig :: GType
gTypeGLConfig =
  {# call fun unsafe gdk_gl_config_get_type #}

-- ***************************************************************** GLDrawable

{#pointer *GdkGLDrawable as GLDrawable foreign newtype #} deriving (Eq,Ord)

mkGLDrawable = (GLDrawable, objectUnrefFromMainloop)
unGLDrawable (GLDrawable o) = o

class GObjectClass o => GLDrawableClass o
toGLDrawable :: GLDrawableClass o => o -> GLDrawable
toGLDrawable = unsafeCastGObject . toGObject

instance GLDrawableClass GLDrawable
instance GObjectClass GLDrawable where
  toGObject = GObject . castForeignPtr . unGLDrawable
  unsafeCastGObject = GLDrawable . castForeignPtr . unGObject

castToGLDrawable :: GObjectClass obj => obj -> GLDrawable
castToGLDrawable = castTo gTypeGLDrawable "GLDrawable"

gTypeGLDrawable :: GType
gTypeGLDrawable =
  {# call fun unsafe gdk_gl_drawable_get_type #}


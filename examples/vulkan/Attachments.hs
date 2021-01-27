{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Attachments where

-- base
import Data.Word
  ( Word32 )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( empty )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Core10.Pass as Vulkan.AttachmentReference
  ( AttachmentReference(..) )
import qualified Vulkan.Zero as Vulkan

-- dear-imgui
import Util
  ( iunzipWith )

---------------------------------------------------------------
-- Attachment types and their corresponding image layouts.

data AttachmentAccess
  = ReadAttachment
  | ReadWriteAttachment
  deriving stock ( Eq, Show )

data DepthStencilType =
  DepthStencilType
    { depth   :: Maybe AttachmentAccess
    , stencil :: Maybe AttachmentAccess
    }
  deriving stock ( Eq, Show )

data InputAttachmentType
  = ColorInputAttachment
  | DepthInputAttachment
  | StencilInputAttachment
  | DepthStencilInputAttachment
  deriving stock ( Eq, Show )

data AttachmentType
  = ColorAttachment
  | DepthStencilAttachment DepthStencilType
  | InputAttachment InputAttachmentType
  deriving stock ( Eq, Show )

data AttachmentUsage
  = UseAttachment
  | PreserveAttachment
  | ResolveAttachment
  deriving stock ( Eq, Show )


depthStencilAttachmentLayout :: DepthStencilType -> Vulkan.ImageLayout
depthStencilAttachmentLayout
  ( DepthStencilType Nothing Nothing )
    = Vulkan.IMAGE_LAYOUT_GENERAL
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadAttachment) Nothing )
    = Vulkan.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadWriteAttachment) Nothing )
    = Vulkan.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType Nothing (Just ReadAttachment) )
    = Vulkan.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType Nothing (Just ReadWriteAttachment) )
    = Vulkan.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadAttachment) (Just ReadAttachment) )
    = Vulkan.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadWriteAttachment) (Just ReadAttachment) )
    = Vulkan.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadAttachment) (Just ReadWriteAttachment) )
    = Vulkan.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  ( DepthStencilType (Just ReadWriteAttachment) (Just ReadWriteAttachment) )
    = Vulkan.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL


inputAttachmentLayout :: InputAttachmentType -> Vulkan.ImageLayout
inputAttachmentLayout ColorInputAttachment
  = Vulkan.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
inputAttachmentLayout DepthInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType (Just ReadAttachment) Nothing )
inputAttachmentLayout StencilInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType Nothing (Just ReadAttachment) )
inputAttachmentLayout DepthStencilInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType (Just ReadAttachment) (Just ReadAttachment) )

attachmentLayout :: AttachmentType -> Vulkan.ImageLayout
attachmentLayout ColorAttachment
  = Vulkan.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
attachmentLayout (DepthStencilAttachment depthStencilType)
  = depthStencilAttachmentLayout depthStencilType
attachmentLayout (InputAttachment inputAttachmentType)
  = inputAttachmentLayout inputAttachmentType

---------------------------------------------------------------
-- Some simple attachment descriptions, for convenience.

presentableColorAttachmentDescription :: Vulkan.Format -> ( Vulkan.AttachmentDescription, AttachmentType )
presentableColorAttachmentDescription colorFormat =
  ( description, ColorAttachment )
  where
    description =
      Vulkan.AttachmentDescription
        { flags          = Vulkan.zero
        , format         = colorFormat
        , samples        = Vulkan.SAMPLE_COUNT_1_BIT
        , loadOp         = Vulkan.ATTACHMENT_LOAD_OP_CLEAR
        , storeOp        = Vulkan.ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp  = Vulkan.ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = Vulkan.ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout  = Vulkan.IMAGE_LAYOUT_UNDEFINED
        , finalLayout    = Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
        

depthAttachmentDescription :: Vulkan.Format -> ( Vulkan.AttachmentDescription, AttachmentType )
depthAttachmentDescription depthFormat =
  ( description, DepthStencilAttachment ( DepthStencilType (Just ReadWriteAttachment) Nothing ) )
    where
      description =
        Vulkan.AttachmentDescription
          { flags          = Vulkan.zero
          , format         = depthFormat
          , samples        = Vulkan.SAMPLE_COUNT_1_BIT
          , loadOp         = Vulkan.ATTACHMENT_LOAD_OP_CLEAR
          , storeOp        = Vulkan.ATTACHMENT_STORE_OP_STORE
          , stencilLoadOp  = Vulkan.ATTACHMENT_LOAD_OP_DONT_CARE
          , stencilStoreOp = Vulkan.ATTACHMENT_STORE_OP_DONT_CARE
          , initialLayout  = Vulkan.IMAGE_LAYOUT_UNDEFINED
          , finalLayout    = Vulkan.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          }

msDepthAttachmentDescription
  :: Vulkan.SampleCountFlagBits
  -> Vulkan.Format
  -> ( Vulkan.AttachmentDescription, AttachmentType )
msDepthAttachmentDescription samples depthFormat =
  ( description, DepthStencilAttachment ( DepthStencilType (Just ReadWriteAttachment) Nothing ) )
    where
      description =
        Vulkan.AttachmentDescription
          { flags          = Vulkan.zero
          , format         = depthFormat
          , samples        = samples
          , loadOp         = Vulkan.ATTACHMENT_LOAD_OP_CLEAR
          , storeOp        = Vulkan.ATTACHMENT_STORE_OP_STORE
          , stencilLoadOp  = Vulkan.ATTACHMENT_LOAD_OP_DONT_CARE
          , stencilStoreOp = Vulkan.ATTACHMENT_STORE_OP_DONT_CARE
          , initialLayout  = Vulkan.IMAGE_LAYOUT_UNDEFINED
          , finalLayout    = Vulkan.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          }

msColorAttachmentDescription
  :: Vulkan.SampleCountFlagBits
  -> Vulkan.Format
  -> ( Vulkan.AttachmentDescription, AttachmentType )
msColorAttachmentDescription samples colorFormat =
  ( description, ColorAttachment )
    where
    description =
      Vulkan.AttachmentDescription
        { flags          = Vulkan.zero
        , format         = colorFormat
        , samples        = samples
        , loadOp         = Vulkan.ATTACHMENT_LOAD_OP_CLEAR
        , storeOp        = Vulkan.ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp  = Vulkan.ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = Vulkan.ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout  = Vulkan.IMAGE_LAYOUT_UNDEFINED
        , finalLayout    = Vulkan.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }

---------------------------------------------------------------
-- Set the attachments in a subpass.

data SubpassAttachments a
  = SubpassAttachments
  { colorAttachments         :: Boxed.Vector a
  , mbDepthStencilAttachment :: Maybe a
  , inputAttachments         :: Boxed.Vector a
  , preserveAttachments      :: Boxed.Vector a
  , resolveAttachments       :: Boxed.Vector a
  } deriving stock ( Functor, Foldable, Traversable )

type SubpassAttachmentReferences = SubpassAttachments Vulkan.AttachmentReference


noAttachments :: SubpassAttachments a
noAttachments =
  SubpassAttachments
  Boxed.Vector.empty
  Nothing
  Boxed.Vector.empty
  Boxed.Vector.empty
  Boxed.Vector.empty 


createSubpass
  :: SubpassAttachmentReferences
  -> Vulkan.SubpassDescription
createSubpass SubpassAttachments { .. } =
  Vulkan.SubpassDescription
    { flags                  = Vulkan.zero
    , colorAttachments       = colorAttachments
    , pipelineBindPoint      = Vulkan.PIPELINE_BIND_POINT_GRAPHICS
    , depthStencilAttachment = mbDepthStencilAttachment
    , inputAttachments       = inputAttachments
    , preserveAttachments    = fmap Vulkan.AttachmentReference.attachment preserveAttachments
    , resolveAttachments     = resolveAttachments
    }

attachmentReference :: Word32 -> AttachmentType -> Vulkan.AttachmentReference
attachmentReference attachmentNumber attachmentType =
  Vulkan.AttachmentReference
    { attachment = attachmentNumber
    , layout     = attachmentLayout attachmentType
    }

attachmentReferencesAndDescriptions
  :: forall t. Traversable t
  => t ( Vulkan.AttachmentDescription, AttachmentType )
  -> ( t Vulkan.AttachmentReference, [ Vulkan.AttachmentDescription ] )
attachmentReferencesAndDescriptions =
  iunzipWith
    ( \ i -> attachmentReference i . snd )
    ( const fst )

module TCP.Message ( Message(..) ) where

import TCP.Chan ( ShowRead(..) )

data Message agent message =
    Message { fromAgent :: agent,
              toAgent :: agent,
              message :: message }
    deriving ( Show, Read )

instance (ShowRead g, ShowRead a) => ShowRead (Message g a)

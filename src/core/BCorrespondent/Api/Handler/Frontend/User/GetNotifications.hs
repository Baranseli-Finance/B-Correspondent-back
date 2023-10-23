{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Frontend.User.GetNotifications (handle) where

import BCorrespondent.Transport.Model.Frontend (Notifications (..), Notification (Notification))
import BCorrespondent.Transport.Response (Response (Ok))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader  -> KatipHandlerM (Response Notifications) 
handle _ = return $ Ok $ Notifications 45 $ zipWith Notification [1..] xs

xs =
    [
        "in the spring of 1889 , the exploring fever came strong on me again , and , seeking the advice of Mr. Ney Elias , a journey across Tibet ",
        "by much the same route as that afterwards so successfully explored by Captain Bower , was suggested to me",
         "I had begun to think over details for this and plan out the journey",
         "when my hopes were utterly shattered by the stern refusal of my commanding officer to allow me to go",
        "left in despair to wile away the dreary hot - weather months in an Indian",
        "In describing my journey from Yarkand to Kashmir in the last two chapters ",
        "The traders on the road from Yarkand to India continually suffered from these wild free booters",
        "the peace - loving inhabitants of the Yarkand valleys were ever subject to their attacks , and compelled to hand them over black - mail",
        "the nomadic Kirghiz , scattered defenceless in their tents over the open valleys of the Pamirs ",
        "had to pay their tribute , or suffer the consequences of refusal ",
        "In the autumn of 1888 - the year after I had crossed the Mustagh Pass",
        "these robbers had made an unusually daring attack upon a large caravan , and had carried off a number of Kirghiz from Shahidula , on the Yarkand road",
        "The Kirghiz had applied to the Chinese for protection against such raids , but had been refused it",
        "It was to inquire into and report upon the circumstances ",
        "the peace - loving inhabitants of the Yarkand valleys were ever subject to their attacks "
    ]


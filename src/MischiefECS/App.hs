module MischiefECS.App where

import MischiefECS.World

data App = App {systems :: [System ()]}

newApp :: App
newApp = App {systems = []}
import Todo.Serve
import Todo.Test

import System.Environment

main = getArgs >>= dispatch

dispatch ["serve"] = serve
dispatch ("test":args) = withArgs args todoTest
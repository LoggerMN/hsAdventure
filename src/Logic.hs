module Logic where
import Model
import View

l = location istate
f = roomFunc l
rs = rooms istate
nrm = (f rs) { visited=True }
nstate = istate { rooms=updateRoom rs l nrm }

showIstate = show nstate ++ "\n\n" ++ (showFull nstate) ++ "\n\n" ++ (showShort nstate)



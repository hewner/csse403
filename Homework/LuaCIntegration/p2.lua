
function doRound(lastRoundOpponentPlay)
    if(lastRoundOpponentPlay) == 0 then
         return(1) -- paper on the first round
    end
    return(lastRoundOpponentPlay) -- do what they do
end

print("Mirror initalized")
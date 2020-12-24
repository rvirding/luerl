local utf8 = require("utf8char")

local chosungPairs = {["ㄱ"]=4352, ["ㄲ"]=4353, ["ㄴ"]=4354, ["ㄷ"]=4355, ["ㄸ"]=4356, ["ㄹ"]=4357, ["ㅁ"]=4358, ["ㅂ"]=4359, ["ㅃ"]=4360, ["ㅅ"]=4361, ["ㅆ"]=4362, ["ㅇ"]=4363, ["ㅈ"]=4364, ["ㅉ"]=4365, ["ㅊ"]=4366, ["ㅋ"]=4367, ["ㅌ"]=4368, ["ㅍ"]=4369, ["ㅎ"]=4370}
local jongsungPairs = {["ㄱ"]=4352, ["ㄲ"]=4353, ["ㄳ"]=4354, ["ㄴ"]=4355, ["ㄵ"]=4356, ["ㄶ"]=4357, ["ㄷ"]=4358, ["ㄹ"]=4359, ["ㄺ"]=4360, ["ㄻ"]=4361, ["ㄼ"]=4362, ["ㄽ"]=4363, ["ㄾ"]=4364, ["ㄿ"]=4365, ["ㅀ"]=4366, ["ㅁ"]=4367, ["ㅂ"]=4368, ["ㅄ"]=4369, ["ㅅ"]=4370, ["ㅆ"]=4371, ["ㅇ"]=4372, ["ㅈ"]=4373, ["ㅊ"]=4374, ["ㅋ"]=4375, ["ㅌ"]=4376, ["ㅍ"]=4377, ["ㅎ"]=4378}

local merge = function(tbl)
    local returnString
    
    assert(#tbl >= 1 and #tbl <= 3, "merge needs 1~3 jamoeums")

    for k,v in pairs(tbl) do
        local byte = utf8.byte(v)

        if k == 1 or k == 3 then
            assert(byte >= 12593 and byte <= 12622, "merge needs korean jaeums as first, third arguments.")
        else
            assert(byte >= 12623 and byte <= 12643, "merge needs korean moeums as second arguments.")
        end
    end

    if(#tbl == 1) then
        returnString = tbl[1]
    elseif(#tbl == 2) then
		print(utf8.byte(tbl[1]))
        local second = utf8.byte(tbl[2]) - 8174
        local indexFirst = chosungPairs[tbl[1]] - 4352
        local indexSecond = second - 4449

        local c = 44032 + 21*28*indexFirst + 28*indexSecond
        returnString = utf8.char(44032 + 21*28*indexFirst + 28*indexSecond)
    elseif(#tbl == 3) then
        local second = utf8.byte(tbl[2]) - 8174

        local indexFirst = chosungPairs[tbl[1]] - 4352
        local indexSecond = second - 4449
        local indexThird = jongsungPairs[tbl[3]] - 4351
        local c = 44032 + (21*indexFirst + indexSecond)*28 + indexThird
        returnString = utf8.char(c)
    end
    
    return returnString
end

local chosungs = {"ㄱ", "ㄲ", "ㄴ", "ㄷ", "ㄸ", "ㄹ", "ㅁ", "ㅂ", "ㅃ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅉ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"}
local i_jaums = {"ㄱ", "ㄲ", "ㄴ", "ㄷ", "ㄸ", "ㄹ", "ㅁ", "ㅂ", "ㅃ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅉ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"}
local i_jaums_jongsung = {"ㄱ","ㄲ","ㄳ","ㄴ","ㄵ","ㄶ","ㄷ","ㄹ","ㄺ","ㄻ","ㄼ","ㄽ","ㄾ","ㄿ","ㅀ","ㅁ","ㅂ","ㅄ","ㅅ","ㅆ","ㅇ","ㅈ","ㅊ","ㅋ","ㅌ","ㅍ","ㅎ"}
local i_moums = {"ㅏ","ㅐ","ㅑ","ㅒ","ㅓ","ㅔ","ㅕ","ㅖ","ㅗ","ㅘ","ㅙ","ㅚ","ㅛ","ㅜ","ㅝ","ㅞ","ㅟ","ㅠ","ㅡ","ㅢ","ㅣ"}

for k,v in pairs(chosungs) do
    chosungs[v] = k
end
for k,v in ipairs(chosungs) do
    chosungs[k] = nil
end

local exists = function(origStr, toFind)
    local byteToFind = utf8.byte(toFind)
    if byteToFind < 12593 or byteToFind > 12622 then
        return utf8.find(origStr, toFind) ~= nil
    end
    local indexToFind = chosungs[toFind] - 1
    if(indexToFind == nil) then
        return utf8.find(origStr, toFind) ~= nil
    end
    
    for ik = 1, utf8.len(origStr) do
        local var = utf8.sub(origStr, ik, ik)
        local base = utf8.byte(var)
        
        local indexFirst = math.floor(((((base-44032) - (base-44032)%28))/28) / 21)
        
        if(indexFirst == indexToFind) then
            return true
        end
    end

    return false
end

local split = function(origStr)
    local returnTable = {}
    for ik = 1, utf8.len(origStr), 1 do
        local bytedStr = utf8.byte(utf8.sub(origStr, ik, ik))
        if bytedStr >= utf8.byte("ㄱ") and bytedStr <= utf8.byte("힣") then
            local indexFirst = math.floor(((((bytedStr-44032) - (bytedStr-44032)%28))/28) / 21) + 1
            local indexSecond = math.floor(((((bytedStr-44032) - (bytedStr-44032)%28))/28) % 21) + 1
            local indexThird = math.floor((bytedStr-44032) % 28)
    
            if i_jaums[indexFirst] == nil then
                returnTable[#returnTable+1] = {utf8.char(bytedStr)}
            elseif indexThird == 0 then
                returnTable[#returnTable+1] = {i_jaums[indexFirst], i_moums[indexSecond]}
            else
                returnTable[#returnTable+1] = {i_jaums[indexFirst], i_moums[indexSecond], i_jaums_jongsung[indexThird]}
            end
        else
            returnTable[#returnTable+1] = {utf8.char(bytedStr)}
        end
    end
    return returnTable
end

local returnTable = {}
for k,v in pairs(utf8) do
    returnTable[k] = v
end
returnTable.merge = merge
returnTable.split = split
returnTable.exists = exists

return returnTable
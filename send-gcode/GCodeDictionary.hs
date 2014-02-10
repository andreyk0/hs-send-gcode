{-# LANGUAGE OverloadedStrings #-}

-- | http://reprap.org/wiki/G-code

module GCodeDictionary where

import Data.Text (Text)

firmwareVersion :: Text
firmwareVersion = "M115"

getExtruderTemperature :: Text
getExtruderTemperature =  "M105"

getCurrentPosition :: Text
getCurrentPosition =  "M114"

emergencyStop :: Text
emergencyStop = "M112"

fanOff :: Text
fanOff = "M107"

extruderTemperatureOff :: Text
extruderTemperatureOff = "M104 S0"

bedTemperatureOff :: Text
bedTemperatureOff = "M140 S0"

disableMotors :: Text
disableMotors = "M84"

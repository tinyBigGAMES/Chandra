--[[---------------------------------------------------------------------------

                  ___  _                    _
                 / __|| |_   __ _  _ _   __| | _ _  __ _ ™
                | (__ | ' \ / _` || ' \ / _` || '_|/ _` |
                 \___||_||_|\__,_||_||_|\__,_||_|  \__,_|
                         Lua Scripting for Pascal

                 Copyright © 2024-present tinyBigGAMES™ LLC
                          All Rights Reserved.

                   https://github.com/tinyBigGAMES/PLUA

                 See LICENSE file for license information
 ----------------------------------------------------------------------------]]

local mymath =  {}

function mymath.add(a,b)
   print(a+b)
end

function mymath.sub(a,b)
   print(a-b)
end

function mymath.mul(a,b)
   print(a*b)
end

function mymath.div(a,b)
   print(a/b)
end                    

function mymath.test()
  print('test')
end

function mymath.test2()
  print('test2')     
end

return mymath    
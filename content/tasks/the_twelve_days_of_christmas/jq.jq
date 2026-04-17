[ "st","nd","rd" ] as $nth |
[ "a partridge in a pear tree.", "turtle doves", "French hens", "calling birds", "gold rings", "geese a-laying", "swans a-swimming", "maids a-milking", "ladies dancing", "lords a-leaping", "pipers piping", "drummers drumming" ] as $gifts |
range(12) | . as $i |
"On the " + (.+1|tostring)+if $i < ($nth|length) then $nth[$i] else "th" end + " day of Christmas, my true love gave to me\n" + if $i > 0 then [[range($i)]|reverse[]|((.+2|tostring) + " " + $gifts[.+1] + if $i > 1 then "," else "" end +"\n")]|join("") + "and " else "" end + $gifts[0] + "\n"

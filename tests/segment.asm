    "REMAINS1"
block
    "01234567"
end
block
    "ABCDEFGH"
end

@(segment :size 256)
@(segment :size 256 :may-be-shorter? t)

    "REMAINS2"

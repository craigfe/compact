include StdLabels
include MoreLabels

module Array = struct
  include Array

  let map_inplace t ~f =
    for i = 0 to Array.length t - 1 do
      Array.unsafe_set t i (f (Array.unsafe_get t i))
    done
end

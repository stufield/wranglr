# no matches returns identical object, with a 1 message & 2 warnings

    Code
      new <- scale_features(short_adat, ref)
    Message
      x No matches between lists
    Condition
      Warning:
      Missing scalar value for (3) features. They will not be transformed.
      Please check the reference or its names.
      Warning:
      There are extra scaling values (1) in the reference.
      They will be ignored.

# `scale_features()` only accepts the `soma_adat` class

    Code
      scale_features(bad_adat)
    Condition
      Error in `scale_features()`:
      ! argument "scale_vec" is missing, with no default


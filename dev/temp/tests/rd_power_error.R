#
set.seed(12345)

### with interaction

## alpha

# sharp
rddapp::rd_power(num.rep = 1000, coeff = c(0, 0, 0.2, 0.1))

# success     mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 -0.0001367901 0.02845634 0.002 0.011 0.057
# Opt       1000 -0.0008995033 0.04518053 0.001 0.016 0.081

# fuzzy
rddapp::rd_power(num.rep = 1000, coeff = c(0, 0, 0.2, 0.1), x.fuzzy = c(0.1, 0.1))

# success    mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 -0.012799184 0.05173406 0.001 0.012 0.061
# Opt       1000 -0.001605643 0.07958140 0.000 0.011 0.064

## power

# sharp
rddapp::rd_power(num.rep = 1000, coeff = c(0, 1, 0.2, 0.1))

# success mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 0.9940856 0.02903799 0.996 0.999 0.999
# Opt       1000 0.9891232 0.04364986 0.934 0.978 0.995

# fuzzy
rddapp::rd_power(num.rep = 1000, coeff = c(0, 1, 0.2, 0.1), x.fuzzy = c(0.1, 0.1))

# success mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 0.9948891 0.05336916 0.880 0.953 0.981
# Opt       1000 0.9892013 0.07901544 0.719 0.868 0.933

### without interaction

## alpha

# sharp
rddapp::rd_power(num.rep = 1000, coeff = c(0, 0, 0.2, 0))

# success   mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 0.005751065 0.02999824 0.001 0.019 0.062
# Opt       1000 0.003787568 0.04891766 0.003 0.023 0.068

# fuzzy
rddapp::rd_power(num.rep = 1000, coeff = c(0, 0, 0.2, 0), x.fuzzy = c(0.1, 0.1))

# success     mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 -0.0009759054 0.04963212 0.001 0.011 0.051
# Opt       1000 -0.0067803251 0.07627443 0.000 0.014 0.056

## power

# sharp
rddapp::rd_power(num.rep = 1000, coeff = c(0, 1, 0.2, 0))

# success mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000  1.012145 0.02803061 0.995 1.000 1.000
# Opt       1000  1.013670 0.04719465 0.941 0.975 0.993

# fuzzy
rddapp::rd_power(num.rep = 1000, coeff = c(0, 1, 0.2, 0), x.fuzzy = c(0.1, 0.1))

# success mean(est)   var(est) 0.001  0.01  0.05
# Linear    1000 0.9956048 0.04574224 0.874 0.959 0.991
# Opt       1000 0.9854766 0.07562496 0.713 0.864 0.937

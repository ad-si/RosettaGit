# Bash has no native float type. Use bc or printf with awk / dc.

# bc respects the requested scale, treating the values as exact decimals.
echo "scale=17; 0.1 + 0.2" | bc      # .3

# awk uses C doubles, so it shows the IEEE 754 result.
awk 'BEGIN { printf "%.17f\n", 0.1 + 0.2 }'   # 0.30000000000000004

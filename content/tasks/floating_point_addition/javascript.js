console.log(0.1 + 0.2);           // 0.30000000000000004
console.log(0.1 + 0.2 === 0.3);   // false

// BigInt only handles integers, so to get exact decimal arithmetic
// you need a library or scale to integers manually:
console.log((0.1 * 10 + 0.2 * 10) / 10);   // 0.3

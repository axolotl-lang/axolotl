// generated by axl

const __plus__int = (...args) => Math.round(__plus__float(args));
const __plus__float = (...args) => args.reduce((acc, curr) => acc + curr);

const __minus__int = (...args) => Math.round(__minus__float(args));
const __minus__float = (...args) => args.reduce((acc, curr) => acc - curr);

const __multiply__int = (...args) => Math.round(__multiply__float(args));
const __multiply__float = (...args) => args.reduce((acc, curr) => acc * curr);

const __divide__int = (...args) => Math.round(__divide__float(args));
const __divide__float = (...args) => args.reduce((acc, curr) => acc / curr);

const __str = (...args) => args.reduce((acc, curr) => acc + curr);
const __print = console.log;

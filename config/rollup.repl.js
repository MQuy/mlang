import typescript from "rollup-plugin-typescript2";
import resolve from "rollup-plugin-node-resolve";
import commonjs from "rollup-plugin-commonjs";

export default {
  input: "src/repl.ts",
  output: {
    file: "build/repl.js",
    format: "cjs",
  },
  plugins: [
    typescript({
      cacheRoot: "./node_modules/.cache/rpt2",
    }),
    resolve({
      browser: true,
    }),
    commonjs(),
  ],
};

import typescript from "rollup-plugin-typescript2";

export default {
  input: "src/index.ts",
  output: {
    file: "repl.js",
    format: "cjs"
  },
  plugins: [
    typescript({
      cacheRoot: "./node_modules/.cache/rpt2"
    })
  ]
};

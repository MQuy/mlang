import typescript from "rollup-plugin-typescript2";

export default {
  input: "src/index.ts",
  output: {
    file: "build/mq.js",
    format: "umd",
    name: "mts",
  },
  plugins: [
    typescript({
      cacheRoot: "./node_modules/.cache/rpt2",
      tsconfig: "./tsconfig.json",
    }),
  ],
};

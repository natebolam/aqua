import {
  main,
  registerA,
  calc,
  calc2,
  ifCalc,
  bugLNG260,
} from "../compiled/examples/funcs.js";

export async function funcsCall() {
  registerA({
    getJ: (n) => {
      return n;
    },
  });

  let res1 = await main((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res2 = await calc((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res3 = await calc2((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res4 = await ifCalc();

  return [res1, res2, res3, res4];
}

export async function bugLNG260Call(a: number, b: number) {
  return await bugLNG260(a, b);
}

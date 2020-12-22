const fs = require("fs");
const MOVE_X_Y_LIST = [
  { x: 1, y: 1 },
  { x: 3, y: 1 },
  { x: 5, y: 1 },
  { x: 7, y: 1 },
  { x: 1, y: 2 },
];

fs.readFile(__dirname + "/day3.txt", "utf8", (err, data) => {
  if (err) console.log(err);
  const lines = data.split("\n");
  const lineLength = lines[0].length;
  const treeCountList = MOVE_X_Y_LIST.map(({ x, y }) => {
    let curX = 0;
    let curY = 0;
    let treeCount = 0;
    while (curY < lines.length - y) {
      curX += x;
      curY += y;
      if (lines[curY].charAt(curX % lineLength) === "#") treeCount++;
    }
    return treeCount;
  });
  console.log("treeCountList: ", treeCountList);
  console.log(
    "result: ",
    treeCountList.reduce((a, b) => a * b)
  );
});

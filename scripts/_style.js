const esc = "\u001b";

const toCamelCase = (...[s, ...xs]) =>
  s + xs.map(s => s.charAt(0).toUpperCase() + s.slice(1)).join("");

const start = (...styles) => `${esc}[${styles.join(";")}m`;

const end = `${esc}[0m`;

const bold = "1";
const italic = "3";
const reverse = "7";

const colorTypes = {
  text: "3",
  textBright: "9",
  bg: "4",
  bgBright: "10"
};

const colors = {
  black: "0",
  red: "1",
  green: "2",
  yellow: "3",
  blue: "4",
  magenta: "5",
  cyan: "6",
  white: "7"
};

const colorStyles = Object.keys(colorTypes)
  .map(typeName => {
    const type = colorTypes[typeName];
    return Object.keys(colors).reduce((colorStyles, colorName) => {
      const color = colors[colorName];
      const name =
        typeName === "text"
          ? colorName
          : typeName === "textBright"
          ? `bright${colorName}`
          : toCamelCase(typeName, colorName);
      return {
        ...colorStyles,
        [name]: type + color
      };
    }, {});
  })
  // flatten
  .reduce((styles, style) => ({ ...styles, ...style }), {});

module.exports = {
  start,
  end,
  bold,
  italic,
  reverse,
  ...colorStyles
};

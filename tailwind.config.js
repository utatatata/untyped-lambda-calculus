module.exports = {
  theme: {
    extend: {}
  },
  variants: {},
  plugins: [
    function({ addVariant, e }) {
      addVariant("checked", ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(`checked${separator}${className}`)}:checked`;
        });
      });
    }
  ]
};

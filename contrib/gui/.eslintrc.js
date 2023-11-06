module.exports = {
  "env": {
    "browser": true,
    "es2021": true
  },
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
  ],
  "overrides": [
    {
      "env": {
        "node": true
      },
      "files": [
        ".eslintrc.{js,cjs}",
        "*.config.js",
      ],
      "parserOptions": {
        "sourceType": "script"
      }
    }
  ],
  "parser": "@typescript-eslint/parser",
  "parserOptions": {
    "ecmaVersion": "latest",
    "sourceType": "module"
  },
  "plugins": [
    "@typescript-eslint",
    "@stylistic",
  ],
  "ignorePatterns": ["node_modules/", "dist/"],
  "rules": {
    "linebreak-style": [ "error", "unix"],
    "@typescript-eslint/no-explicit-any": [ "off" ],
    "@stylistic/indent": [
      "error", 2, {
        "ignoredNodes": ["JSXElement *", "JSXElement"],
        "SwitchCase": 1 }
    ],
    "@stylistic/jsx-indent-props": [ "warn", "first" ],
    "@stylistic/jsx-closing-bracket-location": [ "warn", "after-props" ],
    "@stylistic/jsx-first-prop-new-line": [ "warn", "never" ],
    "@stylistic/quotes": [ "warn", "double" ],
    "@stylistic/semi": [ "error", "always" ],
    "@stylistic/brace-style": [
      "error", "stroustrup", { "allowSingleLine": true }
    ],
    "@stylistic/type-annotation-spacing": [ "error" ],
    "@stylistic/max-len": ["error", { "code": 80 }],
    "@stylistic/no-tabs": ["error"],
    "@stylistic/eol-last": ["error", "always"],
  }
};

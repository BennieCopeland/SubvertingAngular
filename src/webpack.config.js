module.exports = {
  module: {
    rules: [{
      test: /\.fs(x|proj)?$/,
      use: "fable-loader"
    }]
  }
}

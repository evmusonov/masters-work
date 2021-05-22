module.exports = {
  css: { extract: false },
  pluginOptions: {
    quasar: {
      importStrategy: 'kebab',
      rtlSupport: false
    },
    webpackBundleAnalyzer: {
      openAnalyzer: false
    }
  },
  transpileDependencies: [
    'quasar'
  ]
}

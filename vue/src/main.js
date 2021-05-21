import { createApp } from 'vue'
import App from './App.vue'
import axios from 'axios'
import createAuthRefreshInterceptor from 'axios-auth-refresh'
import VueAxios from 'vue-axios'
import router from './router'
import PrimeVue from 'primevue/config'
import Dialog from 'primevue/dialog'
import './assets/theme.css'
import './assets/custom.css'
import 'primevue/resources/primevue.min.css'
import 'primeicons/primeicons.css'
import 'es6-promise/auto'
import store from './modules/store'
import securedRoutes from './modules/secured-routes'

import ConfirmationService from 'primevue/confirmationservice'
import LoadScript from "vue-plugin-load-script"

import Menubar from 'primevue/menubar'
import InputText from 'primevue/inputtext'
import ToastService from 'primevue/toastservice'
import Toast from 'primevue/toast'
import Button from "primevue/button"
import Checkbox from 'primevue/checkbox'
import ConfirmDialog from 'primevue/confirmdialog'
import TabView from 'primevue/tabview'
import TabPanel from 'primevue/tabpanel'
import Dropdown from 'primevue/dropdown'
import Textarea from 'primevue/textarea'

import 'primeflex/primeflex.css'

// Function that will be called to refresh authorization
const refreshAuthLogic = failedRequest => axios.post('/api/check-token', { refreshToken: store.getters.getToken('refreshToken') }).then(tokenRefreshResponse => {
  store.commit({
    type: 'setToken',
    tokenType: 'accessToken',
    value: tokenRefreshResponse.data.accessToken
  });

  failedRequest.response.config.headers['Authorization'] = tokenRefreshResponse.data.accessToken;
  return Promise.resolve();
});

// Instantiate the interceptor (you can chain it as it returns the axios instance)
createAuthRefreshInterceptor(axios, refreshAuthLogic);

axios.interceptors.request.use(
  function (config) {
    const result = securedRoutes.filter(function (a) {
      let result = config.url.match(a.url);
      if (result !== null && a.methods.includes(config.method)) {
        return true;
      }
    });
    
    if (config.url && result.length) {
      config.headers['Authorization'] = store.getters.getToken('accessToken')
    }
    return config;
  },
  function (error) {
    return Promise.reject(error);
  }
);

const app = createApp(App);
app
  .component('Dialog', Dialog)
  .component('Menubar', Menubar)
  .component('InputText', InputText)
  .component('Button', Button)
  .component('Checkbox', Checkbox)
  .component('ConfirmDialog', ConfirmDialog)
  .component('TabView', TabView)
  .component('TabPanel', TabPanel)
  .component('Dropdown', Dropdown)
  .component('Textarea', Textarea);
app
  .use(router)
  .use(VueAxios, axios)
  .use(PrimeVue, {
    locale: {
        accept: 'Да',
        reject: 'Нет',
    }
})
  .use(store)
  .use(ToastService)
  .use(ConfirmationService)
  .use(LoadScript)

app.component('Toast', Toast)

app.mount('#app');
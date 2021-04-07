import { createApp } from 'vue'
import App from './App.vue'
import axios from 'axios'
import createAuthRefreshInterceptor from 'axios-auth-refresh'
import VueAxios from 'vue-axios'
import router from './router'
import PrimeVue from 'primevue/config'
import Dialog from 'primevue/dialog'
import 'primevue/resources/themes/saga-blue/theme.css'
import 'primevue/resources/primevue.min.css'
import 'primeicons/primeicons.css'
import 'es6-promise/auto'
import store from './modules/store'
import securedRoutes from './modules/secured-routes'

import Menubar from 'primevue/menubar'
import InputText from 'primevue/inputtext'
import ToastService from 'primevue/toastservice'
import Toast from 'primevue/toast'
import Button from "primevue/button"

import 'primeflex/primeflex.css'

// Function that will be called to refresh authorization
const refreshAuthLogic = failedRequest => axios.post('/api/check-token', { refreshToken: store.getters.getToken('refreshToken') }).then(tokenRefreshResponse => {
  store.commit({
    type: 'setToken',
    tokenType: 'accessToken',
    value: tokenRefreshResponse.data.accessToken
  });

  localStorage.setItem('token', tokenRefreshResponse.data.accessToken);
  failedRequest.response.config.headers['Authorization'] = tokenRefreshResponse.data.accessToken;
  return Promise.resolve();
});

// Instantiate the interceptor (you can chain it as it returns the axios instance)
createAuthRefreshInterceptor(axios, refreshAuthLogic);

axios.interceptors.request.use(
  function (config) {
    const result = securedRoutes.filter(function (a) {
      let result = config.url.match(a);
      if (result !== null) {
        return true;
      }
    });
    //console.log(result);
    if (config.url && result.length) {
      config.headers['Authorization'] = store.getters.getToken('accessToken')
    }
    return config;
  },
  function (error) {
    return Promise.reject(error);
  }
);

// axios.interceptors.response.use(function (response) {
//   return response;
// }, function (error) {
//   if (error.response.status == "401") {
//     axios.post('/api/check-token', { refreshToken: store.getters.getToken('refreshToken') })
//       .then((res) => {
//         if (res.data.accessToken) {
//           store.commit({
//             type: 'setToken',
//             tokenType: 'accessToken',
//             value: res.data.accessToken
//           });

//           error.response.config.headers['Authorization'] = res.data.accessToken;
//           return new Promise((resolve, reject) => {
//             axios.request(error.response.config).then(response => {
//               resolve(response);
//             }).catch((error) => {
//               reject(error);
//             })
//           });
//         }
//       });
//   }

//   return Promise.reject(error);
// });


const app = createApp(App);
app
  .component('Dialog', Dialog)
  .component('Menubar', Menubar)
  .component('InputText', InputText)
  .component('Button', Button);
app
  .use(router)
  .use(VueAxios, axios)
  .use(PrimeVue)
  .use(store)
  .use(ToastService)

app.component('Toast', Toast)

app.mount('#app');
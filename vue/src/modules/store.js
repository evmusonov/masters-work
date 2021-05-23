import { createStore } from 'vuex'
import axios from 'axios'

const store = createStore({
  state() {
    return {
      accessToken: '',
      refreshToken: '',
      user: {},
      userLoaded: false,
    }
  },
  mutations: {
    setToken(state, payload) {
      state[payload.tokenType] = payload.value
      localStorage.setItem(payload.tokenType, payload.value);
    },
    setTokenFromStorage(state, payload) {
      if (localStorage.getItem(payload.tokenType)) {
        state[payload.tokenType] = localStorage.getItem(payload.tokenType)
      }
    },
    setUser(state, payload) {
      state.user = payload.user;
      state.userLoaded = true;
    },
  },
  getters: {
    getToken: (state) => (tokenType) => {
      return state[tokenType]
    },
    isAuth(state) {
      return state['accessToken'] !== '';
    },
    getUser(state) {
      return state.user
    },
    getUserLoadingStatus(state) {
      return state.userLoaded;
    },
    isLogged: (state, getters) => {
      if (getters.isAuth) {
        return getters.getUserLoadingStatus;
      } else {
        return true;
      }
    }
  },
  actions: {
    setUserFromDb({ commit }) {
      return axios.get("/api/user").then(response => {
        commit({
          type: "setUser",
          user: response.data,
        });
        return response;
      })
    }
  }
})

export default store;
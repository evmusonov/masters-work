import { createStore } from 'vuex'
import axios from 'axios'

const store = createStore({
  state() {
    return {
      accessToken: '',
      refreshToken: '',
      user: {}
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
      state.user = payload.user
    },
  },
  getters: {
    getToken: (state) => (tokenType) => {
      return state[tokenType]
    },
    isAuth(state) {
      return state.accessToken !== ''
    },
    getUser(state) {
      return state.user
    },
  },
  actions: {
    setUserFromDb ({ commit }) {
      axios.get("/api/user").then(function(res) {
        commit({
          type: "setUser",
          user: res.data
        });
      });
    }
  }
})

export default store;
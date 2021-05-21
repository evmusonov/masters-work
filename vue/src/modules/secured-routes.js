const securedRoutes = [
  {
    url: /^\/api\/users[/]{0,1}[\w]*$/,
    methods: ['get']
  },
  {
    url: /^\/api\/user[/]{0,1}[\w]*$/i,
    methods: ['get', 'put']
  },
  {
    url: /^\/api\/courses$/,
    methods: ['get', 'post']
  },
  {
    url: /^\/api\/courses[\w]*$/,
    methods: ['get', 'put', 'delete']
  },
  {
    url: /^\/api\/ontos$/,
    methods: ['get', 'post']
  },
  {
    url: /^\/api\/ontos[/]{0,1}[\w]*$/,
    methods: ['get', 'put', 'delete']
  },
  {
    url: /^\/api\/users\/favs\/courses\/[\w]*$/,
    methods: ['get', 'post', 'delete']
  },
  {
    url: /^\/api\/users\/courses\/[\w]*$/,
    methods: ['post', 'delete']
  },
  {
    url: /^\/api\/users\/courses\/[\w]*$/,
    methods: ['get']
  },
  {
    url: /^\/api\/users\/courses\/[\w]*\/tests$/,
    methods: ['post']
  },
  {
    url: /^\/api\/users\/courses\/[\w]*\/tests\/[\w]*$/,
    methods: ['get']
  },
  {
    url: /^\/api\/users\/courses\/[\w]*\/tests\/[\w]*\/answers$/,
    methods: ['put']
  },
];

export default securedRoutes;

const securedRoutes = [
  '/api/users',
  /\/api\/user[/]{0,1}[\w]{0,}/i,
];

export default securedRoutes;

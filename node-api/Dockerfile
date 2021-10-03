FROM node:14

WORKDIR /var/www/html/node-api

COPY package*.json ./

RUN npm install && \
 npm install -g nodemon -y && \
 npm install --save express-validator -y \
 npm install --save mysql \
 npm install --save mongoose \
 npm install --save bcrypt

EXPOSE 8070

CMD ["npm", "run", "dev"]

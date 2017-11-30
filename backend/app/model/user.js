'use strict';

const Sequelize = require('sequelize');

module.exports.define = (sequelize) => {
  return sequelize.define('user', {
    code: { type: Sequelize.INTEGER, allowNull: false, unique: true },
    name: { type: Sequelize.STRING },
    tel: { type: Sequelize.STRING }
  }, {
    timestamps: true,
    createdAt: 'created_at',
    updatedAt: 'updated_at'
  });
};

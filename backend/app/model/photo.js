'use strict';

const Sequelize = require('sequelize');

module.exports.define = (sequelize) =>
  sequelize.define('photo', {
    key: { type: Sequelize.STRING },
    original_url: { type: Sequelize.STRING },
    thumbnail_url: { type: Sequelize.STRING }
  }, {
    timestamps: true,
    createdAt: 'created_at',
    updatedAt: 'updated_at',
    underscored: true
  });


module.exports.relate = (m) => {
  if (m.Photo && m.User) {
    m.Photo.belongsTo(m.User);
  }
};

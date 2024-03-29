const Sequelize = require('sequelize');

module.exports.define = (sequelize) => sequelize.define('crop', {
  label: { type: Sequelize.STRING },
}, {
  timestamps: true,
  createdAt: 'created_at',
  updatedAt: 'updated_at',
  underscored: true,
});

module.exports.relate = (sequelize) => {};

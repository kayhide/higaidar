const Sequelize = require('sequelize');

module.exports.define = (sequelize) => sequelize.define('user', {
  code: { type: Sequelize.INTEGER, allowNull: false, unique: true },
  name: { type: Sequelize.STRING },
  tel: { type: Sequelize.STRING },
  is_admin: { type: Sequelize.BOOLEAN },
  is_editor: { type: Sequelize.BOOLEAN },
}, {
  timestamps: true,
  createdAt: 'created_at',
  updatedAt: 'updated_at',
  underscored: true,
});

module.exports.relate = (m) => {
  if (m.User && m.Photo) {
    m.User.hasMany(m.Photo);
  }
};

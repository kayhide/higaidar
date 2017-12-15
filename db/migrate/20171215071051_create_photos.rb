class CreatePhotos < ActiveRecord::Migration[5.1]
  def change
    create_table :photos do |t|
      t.references :user, foreign_key: true
      t.string :key
      t.string :original_url
      t.string :thumbnail_url

      t.timestamps
    end
  end
end

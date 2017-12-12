class CreatePhotos < ActiveRecord::Migration[5.1]
  def change
    create_table :photos do |t|
      t.references :user, foreign_key: true
      t.boolean :is_uploaded, null: false, default: false
      t.string :original_url
      t.string :thumbnail_url

      t.timestamps
    end
  end
end

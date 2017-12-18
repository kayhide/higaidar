class AddPestToPhotos < ActiveRecord::Migration[5.1]
  def change
    add_column :photos, :pest, :string
  end
end

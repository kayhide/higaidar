class CreatePests < ActiveRecord::Migration[5.1]
  def change
    create_table :pests do |t|
      t.string :label

      t.timestamps
    end
  end
end

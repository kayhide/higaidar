class CreateCrops < ActiveRecord::Migration[5.1]
  def change
    create_table :crops do |t|
      t.string :label

      t.timestamps
    end

    add_column :photos, :crop, :string
    add_column :pests, :crop, :string
  end
end

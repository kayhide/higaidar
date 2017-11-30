class CreateUsers < ActiveRecord::Migration[5.1]
  def change
    create_table :users do |t|
      t.integer :code, null: false
      t.string :name
      t.string :tel

      t.timestamps
    end
  end
end

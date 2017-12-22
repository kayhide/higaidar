class ChangeCodeOfUsers < ActiveRecord::Migration[5.1]
  def change
    change_column :users, :code, :string
  end
end

class AddIsEditorToUsers < ActiveRecord::Migration[5.1]
  def change
    add_column :users, :is_editor, :boolean, null: false, default: false
  end
end
